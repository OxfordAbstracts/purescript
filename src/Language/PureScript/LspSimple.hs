{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Language.PureScript.LspSimple (main) where

import Control.Lens (Field1 (_1), (^.))
import Control.Lens.Getter (to)
import Control.Monad.IO.Unlift
import Control.Monad.Reader (mapReaderT)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.List.NonEmpty qualified as NEL
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Utf16.Rope.Mixed as Rope
import Data.Time (getCurrentTime)
import Debug.Trace (traceM)
import GHC.Float (Floating (log))
import GHC.IO (unsafePerformIO)
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as Message
import Language.LSP.Protocol.Types (Diagnostic, UInt, Uri)
import Language.LSP.Protocol.Types qualified as Types
import Language.LSP.Server (getConfig)
import Language.LSP.Server qualified as Server
import Language.LSP.VFS qualified as VFS
import Language.PureScript (accumTypes, prettyPrintType)
import Language.PureScript qualified as P
import Language.PureScript.AST.Declarations (declSourceAnn, getModuleDeclarations, getModuleName)
import Language.PureScript.Docs.Convert.Single (convertComments)
import Language.PureScript.Errors (ErrorMessage (ErrorMessage), MultipleErrors (runMultipleErrors), errorCode, errorDocUri, errorSpan, noColorPPEOptions, prettyPrintSingleError)
import Language.PureScript.Errors qualified as Errors
import Language.PureScript.Errors.JSON (toSuggestion)
import Language.PureScript.Errors.JSON qualified as JsonErrors
import Language.PureScript.Externs (ExternsFile (efSourceSpan))
import Language.PureScript.Ide (findAvailableExterns, loadModulesAsync)
import Language.PureScript.Ide.Completion (getCompletions, getExactCompletions)
import Language.PureScript.Ide.Completion qualified as Purs.Completion
import Language.PureScript.Ide.Error (IdeError (RebuildError), prettyPrintTypeSingleLine, textError)
import Language.PureScript.Ide.Filter (Filter, moduleFilter)
import Language.PureScript.Ide.Imports (parseImportsFromFile)
import Language.PureScript.Ide.Logging (runErrLogger)
import Language.PureScript.Ide.Matcher (Matcher)
import Language.PureScript.Ide.Prim (idePrimDeclarations)
import Language.PureScript.Ide.State (getAllModules)
import Language.PureScript.Ide.Types (Completion (Completion, complDocumentation, complExpandedType, complType), IdeDeclarationAnn)
import Language.PureScript.Ide.Util (runLogger)
import Language.PureScript.Lsp.Cache (dropTables, initDb, insertAllExterns)
import Language.PureScript.Lsp.Cache.Query (getDeclaration)
import Language.PureScript.Lsp.Rebuild (rebuildAllFiles, rebuildFile)
import Language.PureScript.Lsp.State (cachedRebuild)
import Language.PureScript.Lsp.Types (CurrentFile (CurrentFile), LspConfig (confLogLevel, confOutputPath), LspEnvironment (lspConfig, lspDbConnection))
import Language.PureScript.Lsp.Util (efDeclComments, efDeclSourceSpan, efDeclSourceType, getWordAt)
import Protolude hiding (to)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import Text.PrettyPrint.Boxes (render)
import "monad-logger" Control.Monad.Logger (LoggingT, mapLoggingT)

-- import Language.Haskell.LSP.VFS qualified as VFS

type HandlerM config = Server.LspT config (ReaderT LspEnvironment (LoggingT IO))

type LspM = ReaderT LspEnvironment (LoggingT (ExceptT IdeError IO))

fromLsp :: LspM a -> HandlerM config a
fromLsp = lift . mapReaderT (mapLoggingT (throwIdeError <=< runExceptT))
  where
    throwIdeError = \case
      Left err -> liftIO $ throwIO err
      Right a -> pure a

fromLspWithErr :: LspM a -> HandlerM config (Either IdeError a)
fromLspWithErr = lift . mapReaderT (mapLoggingT runExceptT)

type DiagnosticErrors = IORef (Map Diagnostic ErrorMessage)

insertDiagnosticError :: (MonadIO m, Ord k) => IORef (Map k a) -> a -> k -> m ()
insertDiagnosticError diagErrs err diag = liftIO $ modifyIORef diagErrs (Map.insert diag err)

insertDiagnosticErrors :: (MonadIO m, Ord k) => IORef (Map k a) -> [a] -> [k] -> m ()
insertDiagnosticErrors diagErrs errs diags = liftIO $ modifyIORef diagErrs (Map.union $ Map.fromList $ zip diags errs)

getDiagnosticError :: (MonadIO m, Ord k) => IORef (Map k a) -> k -> m (Maybe a)
getDiagnosticError diagErrs diags = liftIO $ Map.lookup diags <$> readIORef diagErrs

getDiagnosticErrors :: (MonadIO m, Ord k) => IORef (Map k a) -> [k] -> m (Map k a)
getDiagnosticErrors diagErrs diags = liftIO $ flip Map.restrictKeys (Set.fromList diags) <$> readIORef diagErrs

handlers :: DiagnosticErrors -> Server.Handlers (HandlerM ())
handlers diagErrs =
  mconcat
    [ Server.notificationHandler Message.SMethod_Initialized $ \_not -> do
        res <- fromLspWithErr do
          dropTables
          initDb
          insertAllExterns
          rebuildAllFiles
        case res of
          Left err -> do
            log_ err
            sendInfoMsg $ show err
          Right _ -> sendInfoMsg "OA purs lsp server initialized",
      Server.notificationHandler Message.SMethod_TextDocumentDidOpen $ \msg -> do
        sendInfoMsg "TextDocumentDidOpen"
        let uri :: Uri
            uri = getMsgUri msg
            fileName = Types.uriToFilePath uri
        void $ fromLsp $ traverse rebuildFile fileName,
      Server.notificationHandler Message.SMethod_TextDocumentDidChange $ \msg -> do
        sendInfoMsg "TextDocumentDidChange",
      Server.notificationHandler Message.SMethod_TextDocumentDidSave $ \msg -> do
        sendInfoMsg "SMethod_TextDocumentDidSave"
        let uri :: Uri
            uri = getMsgUri msg
            fileName = Types.uriToFilePath uri
        void $ fromLsp $ traverse rebuildFile fileName,
      Server.notificationHandler Message.SMethod_WorkspaceDidChangeConfiguration $ \msg -> do
        cfg <- getConfig
        sendInfoMsg $ "Config changed: " <> show cfg,
      Server.notificationHandler Message.SMethod_SetTrace $ \msg -> do
        sendInfoMsg "SMethod_SetTrace",
      Server.requestHandler Message.SMethod_TextDocumentDiagnostic $ \req res -> do
        sendInfoMsg "SMethod_TextDocumentDiagnostic"
        (errs, diagnostics) <- getFileDiagnotics req
        sendInfoMsg $ "Errors: " <> show errs
        sendInfoMsg $ "diagnostics: " <> show diagnostics
        insertDiagnosticErrors diagErrs errs diagnostics
        res $
          Right $
            Types.DocumentDiagnosticReport $
              Types.InL $
                Types.RelatedFullDocumentDiagnosticReport Types.AString Nothing diagnostics Nothing,
      Server.requestHandler Message.SMethod_TextDocumentCodeAction $ \req res -> do
        sendInfoMsg "SMethod_TextDocumentCodeAction"
        let params = req ^. LSP.params
            diags = params ^. LSP.context . LSP.diagnostics
            uri = getMsgUri req

        errs <- Map.toList <$> getDiagnosticErrors diagErrs diags

        res $
          Right $
            Types.InL $
              errs & fmap \(diag, err) ->
                let textEdits :: [Types.TextEdit]
                    textEdits =
                      toSuggestion err
                        & maybeToList
                          >>= suggestionToEdit

                    suggestionToEdit :: JsonErrors.ErrorSuggestion -> [Types.TextEdit]
                    suggestionToEdit (JsonErrors.ErrorSuggestion replacement (Just errorPos@JsonErrors.ErrorPosition {..})) =
                      let start = Types.Position (fromIntegral $ startLine - 1) (fromIntegral $ startColumn - 1)
                          end = Types.Position (fromIntegral $ endLine - 1) (fromIntegral $ endColumn - 1)
                       in pure $ Types.TextEdit (Types.Range start end) replacement
                    suggestionToEdit _ = []
                 in Types.InR $
                      Types.CodeAction
                        "Apply suggestion"
                        (Just Types.CodeActionKind_QuickFix)
                        (Just diags)
                        (Just True)
                        Nothing -- disabled
                        ( Just $
                            Types.WorkspaceEdit
                              (Just $ Map.singleton uri textEdits)
                              Nothing
                              Nothing
                        )
                        Nothing
                        Nothing,
      Server.requestHandler Message.SMethod_TextDocumentHover $ \req res -> do
        let Types.HoverParams docIdent pos@(Types.Position line col) _workDone = req ^. LSP.params
            filePathMb = Types.uriToFilePath $ docIdent ^. LSP.uri
            docUri =
              docIdent
                ^. LSP.uri
                  . to Types.toNormalizedUri
            nullRes = res $ Right $ Types.InR Types.Null
            forLsp :: Maybe a -> (a -> HandlerM () ()) -> HandlerM () ()
            forLsp val f = maybe nullRes f val
        vfMb <- Server.getVirtualFile docUri

        forLsp vfMb \vf -> do
          let word = getWordAt (VFS._file_text vf) pos
          if word == ""
            then nullRes
            else do
              cacheMb <- fromLsp cachedRebuild
              forLsp cacheMb $ \(CurrentFile _ module' ex) -> do
                declMb <- fromLsp $ getDeclaration (getModuleName module') word
                forLsp declMb $ \decl -> do
                  let declSpan = efDeclSourceSpan decl
                      declType = prettyPrintTypeSingleLine $ efDeclSourceType decl
                      declComments = efDeclComments decl
                      hoverInfo =
                        Types.InL $
                          Types.Hover
                            ( Types.InL $
                                Types.MarkupContent
                                  Types.MarkupKind_Markdown
                                  ( "```purescript\n"
                                      <> word
                                      <> " :: "
                                      <> declType
                                      <> "\n"
                                      <> fold (convertComments declComments)
                                      <> "\n```"
                                  )
                            )
                            Nothing
                  res $ Right hoverInfo
                  -- let moduleName' = case cache of
                  --       Just (CurrentFile mName _) -> Just mName
                  --       _ -> Nothing

                  -- imports <-
                  --   filePathMb
                  --     & maybe (pure Nothing) (fromLsp . parseImportsFromFile)

                  -- let filters :: [Filter]
                  --     filters =
                  --       imports
                  --         & maybe [] (pure . (moduleFilter . insertCurrentModule . Set.fromList . fmap getInputModName) . snd)

                  --     getInputModName (n, _, _) = n

                  --     insertCurrentModule :: Set P.ModuleName -> Set P.ModuleName
                  --     insertCurrentModule mods = maybe mods (flip Set.insert mods) moduleName'

                  -- completions <- fromLsp $ getExactCompletionsWithPrim word filters moduleName'

                  -- let hoverInfo = case head <$> completions of
                  --       Right (Just completion) -> completionToHoverInfo word completion
                  --       _ -> word

                  -- res $
                  --   Right $
                  --     Types.InL $
                  --       Types.Hover
                  --         ( Types.InL $
                  --             Types.MarkupContent Types.MarkupKind_Markdown hoverInfo
                  --         )
                  --         Nothing
                  --             ,
                  -- Server.requestHandler Message.SMethod_TextDocumentDefinition $ \req res -> do
                  --   sendInfoMsg "SMethod_TextDocumentDefinition"
                  --   let Types.DefinitionParams docIdent pos _prog _prog' = req ^. LSP.params
                  --       filePathMb = Types.uriToFilePath $ docIdent ^. LSP.uri
                  --       uri =
                  --         req
                  --           ^. LSP.params
                  --             . LSP.textDocument
                  --             . LSP.uri
                  --             . to Types.toNormalizedUri

                  --       nullRes = res $ Right $ Types.InR $ Types.InR Types.Null

                  --   vfMb <- Server.getVirtualFile uri

                  --   for_ vfMb \vf -> do
                  --     let word = getWordAt (VFS._file_text vf) pos
                  --     cache <- fromLsp cachedRebuild
                  --     let moduleName' = case cache of
                  --           Right (Just (mName, _)) -> Just mName
                  --           _ -> Nothing

                  --     imports <-
                  --       filePathMb
                  --         & maybe (pure Nothing) (fmap hush . fromLsp . parseImportsFromFile)

                  --     let filters :: [Filter]
                  --         filters =
                  --           imports
                  --             & maybe [] (pure . (moduleFilter . insertCurrentModule . Set.fromList . fmap getInputModName) . snd)

                  --         getInputModName (n, _, _) = n

                  --         insertCurrentModule :: Set P.ModuleName -> Set P.ModuleName
                  --         insertCurrentModule mods = maybe mods (flip Set.insert mods) moduleName'

                  --     completions :: Either IdeError [Completion] <- fromLsp $ getExactCompletionsWithPrim word filters moduleName'

                  --     sendInfoMsg $ "Completions: " <> show completions
                  --     let withLocation =
                  --           fold completions
                  --             & mapMaybe
                  --               ( \c -> case complLocation c of
                  --                   Just loc -> Just (c, loc)
                  --                   Nothing -> Nothing
                  --               )
                  --             & head

                  --     paths <- fromLsp $ Map.map snd . fsModules <$> getFileState

                  --     case withLocation of
                  --       Just (completion, location) -> do
                  --         let fpMb =
                  --               Map.lookup (P.ModuleName . complModule $ completion) (either mempty identity paths)

                  --         case fpMb of
                  --           Nothing -> do
                  --             sendInfoMsg "No file path for module"
                  --             nullRes
                  --           Just fp ->
                  --             res $
                  --               Right $
                  --                 Types.InL $
                  --                   Types.Definition $
                  --                     Types.InL $
                  --                       Types.Location
                  --                         (Types.filePathToUri fp)
                  --                         (spanToRange location)
                  --       _ -> do
                  --         sendInfoMsg "No location for completion"
                  --         nullRes
    ]
  where
    getFileDiagnotics :: (LSP.HasUri a2 Uri, LSP.HasTextDocument a1 a2, LSP.HasParams s a1) => s -> HandlerM config ([ErrorMessage], [Types.Diagnostic])
    getFileDiagnotics msg = do
      let uri :: Uri
          uri = getMsgUri msg
          fileName = Types.uriToFilePath uri
      logT $ "Rebuilding file: " <> show (uri, fileName)
      case fileName of
        Just file -> do
          res <- fmap snd <$> fromLspWithErr (rebuildFile file)
          logT $ "Rebuild result: " <> show res
          getResultDiagnostics res
        Nothing -> do
          sendInfoMsg $ "No file path for uri: " <> show uri
          pure ([], [])

    getMsgUri :: (LSP.HasParams s a1, LSP.HasTextDocument a1 a2, LSP.HasUri a2 a3) => s -> a3
    getMsgUri msg = msg ^. LSP.params . LSP.textDocument . LSP.uri

    getResultDiagnostics :: Either IdeError MultipleErrors -> HandlerM config ([ErrorMessage], [Types.Diagnostic])
    getResultDiagnostics res = case res of
      Left (RebuildError _ errs) -> do
        let errors = runMultipleErrors errs
            diags = errorMessageDiagnostic Types.DiagnosticSeverity_Error <$> errors
        pure (errors, diags)
      Left err -> do
        sendError err
        pure ([], [])
      Right errs | Errors.nonEmpty errs -> do
        let errors = runMultipleErrors errs
            diags = errorMessageDiagnostic Types.DiagnosticSeverity_Warning <$> errors
        pure (errors, diags)
      _ -> pure ([], [])
      where
        errorMessageDiagnostic :: Types.DiagnosticSeverity -> ErrorMessage -> Types.Diagnostic
        errorMessageDiagnostic severity msg@((ErrorMessage hints _)) =
          Types.Diagnostic
            (Types.Range start end)
            (Just severity)
            (Just $ Types.InR $ errorCode msg)
            (Just $ Types.CodeDescription $ Types.Uri $ errorDocUri msg)
            (T.pack <$> spanName)
            (T.pack $ render $ prettyPrintSingleError noColorPPEOptions msg)
            Nothing
            Nothing
            Nothing
          where
            notFound = Types.Position 0 0
            (spanName, start, end) = getPositions $ errorSpan msg

            getPositions = fromMaybe (Nothing, notFound, notFound) . getPositionsMb

            getPositionsMb = fmap $ \spans ->
              let (Errors.SourceSpan name (Errors.SourcePos startLine startCol) (Errors.SourcePos endLine endCol)) =
                    NEL.head spans
               in ( Just name,
                    Types.Position (fromIntegral $ startLine - 1) (fromIntegral $ startCol - 1),
                    Types.Position (fromIntegral $ endLine - 1) (fromIntegral $ endCol - 1)
                  )

spanToRange :: Errors.SourceSpan -> Types.Range
spanToRange (Errors.SourceSpan _ (Errors.SourcePos startLine startCol) (Errors.SourcePos endLine endCol)) =
  Types.Range
    (Types.Position (fromIntegral $ startLine - 1) (fromIntegral $ startCol - 1))
    (Types.Position (fromIntegral $ endLine - 1) (fromIntegral $ endCol - 1))

sendError :: IdeError -> HandlerM config ()
sendError err =
  Server.sendNotification
    Message.SMethod_WindowShowMessage
    ( Types.ShowMessageParams Types.MessageType_Error $
        "Something went wrong:\n" <> textError err
    )

-- rebuildFile :: FilePath -> LspM Success
-- rebuildFile file = do
--   rebuildFile file  mempty

sendInfoMsg :: (Server.MonadLsp config f) => Text -> f ()
sendInfoMsg msg = Server.sendNotification Message.SMethod_WindowShowMessage (Types.ShowMessageParams Types.MessageType_Info msg)

main :: LspEnvironment -> IO Int
main lspEnv = do
  diagErrs <- newIORef Map.empty
  Server.runServer $
    Server.ServerDefinition
      { parseConfig = const $ const $ Right (),
        onConfigChange = const $ pure (),
        defaultConfig = (),
        configSection = "oa-purescript-simple",
        doInitialize = \env _req -> do
          logT "Init OA purs lsp server"
          pure $ Right env,
        staticHandlers = \_caps -> do handlers diagErrs,
        interpretHandler = \env ->
          Server.Iso
            ( runErrLogger (confLogLevel (lspConfig lspEnv))
                . flip runReaderT lspEnv
                . Server.runLspT env
            )
            liftIO,
        options = lspOptions
      }

syncOptions :: Types.TextDocumentSyncOptions
syncOptions =
  Types.TextDocumentSyncOptions
    { Types._openClose = Just True,
      Types._change = Just Types.TextDocumentSyncKind_Incremental,
      Types._willSave = Just False,
      Types._willSaveWaitUntil = Just False,
      Types._save = Just $ Types.InR $ Types.SaveOptions $ Just False
    }

lspOptions :: Server.Options
lspOptions =
  Server.defaultOptions
    { Server.optTextDocumentSync = Just syncOptions,
      Server.optExecuteCommandCommands = Just ["lsp-purescript-command"]
    }

spy :: (Show a) => Text -> a -> a
spy msg a = unsafePerformIO $ do
  logT $ msg <> ": " <> show a
  pure a

unsafeLog :: (Show a) => a -> ()
unsafeLog a = unsafePerformIO $ log_ a

log_ :: (MonadIO m, Show a) => a -> m ()
log_ = logToFile . show

logT :: (MonadIO m) => Text -> m ()
logT = logToFile

logToFile :: (MonadIO m) => Text -> m ()
logToFile txt =
  liftIO $
    catchError
      ( do
          createDirectoryIfMissing True "logs"
          time <- show <$> getCurrentTime
          writeFile ("logs/" <> time <> "-----" <> T.unpack txt) $ txt <> "\n"
      )
      (const $ pure ())

-- getCompletionsWithPrim ::
--   (Ide m) =>
--   [Filter] ->
--   Matcher IdeDeclarationAnn ->
--   Maybe P.ModuleName ->
--   Purs.Completion.CompletionOptions ->
--   m [Completion]
-- getCompletionsWithPrim filters matcher currentModule complOptions = do
--   modules <- getAllModules currentModule
--   let insertPrim = Map.union idePrimDeclarations
--   pure (getCompletions filters matcher complOptions (insertPrim modules))

-- getExactCompletionsWithPrim ::
--   (Ide m) =>
--   Text ->
--   [Filter] ->
--   Maybe P.ModuleName ->
--   m [Completion]
-- getExactCompletionsWithPrim search filters currentModule = do
--   modules <- getAllModules currentModule
--   let insertPrim = Map.union idePrimDeclarations
--   pure (getExactCompletions search filters (insertPrim modules))
-- z = getAllModules

completionToHoverInfo :: Text -> Completion -> Text
completionToHoverInfo word Completion {..} =
  typeStr <> "\n" <> fromMaybe "" complDocumentation
  where
    typeStr =
      "```purescript\n"
        <> compactTypeStr
        <> (if showExpanded then "\n" <> expandedTypeStr else "")
        <> "\n```"
    showExpanded = complExpandedType /= "" && (complExpandedType /= complType)
    compactTypeStr = word <> " :: " <> complType
    expandedTypeStr = word <> " :: " <> complExpandedType
