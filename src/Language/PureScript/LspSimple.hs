{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
-- {-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Language.PureScript.LspSimple (main) where

import Control.Lens ((^.))
import Control.Lens.Getter (to)
import Control.Monad.IO.Unlift
import Control.Monad.Reader (mapReaderT)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.List.NonEmpty qualified as NEL
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import GHC.IO (unsafePerformIO)
import Language.Haskell.TH (listT)
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as Message
import Language.LSP.Protocol.Types (Diagnostic, Uri)
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Protocol.Types qualified as Types
import Language.LSP.Server (getConfig)
import Language.LSP.Server qualified as Server
import Language.LSP.VFS qualified as VFS
import Language.PureScript qualified as P
import Language.PureScript.AST.SourcePos (SourcePos (sourcePosColumn))
import Language.PureScript.Constants.TH (ty)
import Language.PureScript.CoreFn.Expr qualified as CF
import Language.PureScript.Docs.Convert.Single (convertComments)
import Language.PureScript.Errors (ErrorMessage (ErrorMessage), MultipleErrors (runMultipleErrors), errorCode, errorDocUri, errorSpan, noColorPPEOptions, prettyPrintSingleError)
import Language.PureScript.Errors qualified as Errors
import Language.PureScript.Errors.JSON (toSuggestion)
import Language.PureScript.Errors.JSON qualified as JsonErrors
import Language.PureScript.Ide.Error (IdeError (GeneralError, RebuildError), prettyPrintTypeSingleLine, textError)
import Language.PureScript.Ide.Logging (runErrLogger)
import Language.PureScript.Ide.Types (Completion (Completion, complDocumentation, complExpandedType, complType), IdeLogLevel (LogAll))
import Language.PureScript.Lsp.Cache (selectExternModuleNameFromFilePath, selectExternPathFromModuleName)
import Language.PureScript.Lsp.Cache.Query (getCoreFnExprAt, getEfDeclarationInModule, getEfDeclarationsAtSrcPos)
import Language.PureScript.Lsp.Docs (readDeclarationDocsAsMarkdown)
import Language.PureScript.Lsp.Print (printDeclarationType, printName)
import Language.PureScript.Lsp.Rebuild (rebuildFile)
import Language.PureScript.Lsp.State (initFinished, waitForInit)
import Language.PureScript.Lsp.Types (LspEnvironment)
import Language.PureScript.Lsp.Util (efDeclComments, efDeclSourceSpan, efDeclSourceType, getNamesAtPosition, getWordAt, lookupTypeInEnv, sourcePosToPosition)
import Language.PureScript.Names (disqualify, runIdent)
import Language.PureScript.Names qualified as P
import Protolude hiding (to)
import System.Directory (createDirectoryIfMissing)
import Text.PrettyPrint.Boxes (render)
import "monad-logger" Control.Monad.Logger (LoggingT, logDebug, logDebugN, logErrorN, logWarnN, mapLoggingT)

type HandlerM config = Server.LspT config (ReaderT LspEnvironment (LoggingT IO))

type LspM = ReaderT LspEnvironment (LoggingT (ExceptT IdeError IO))

liftLsp :: LspM a -> HandlerM config a
liftLsp = lift . mapReaderT (mapLoggingT (throwIdeError <=< runExceptT))
  where
    throwIdeError = \case
      Left err -> liftIO $ throwIO err
      Right a -> pure a

liftLspWithErr :: LspM a -> HandlerM config (Either IdeError a)
liftLspWithErr = lift . flip catchError errorHandler . mapReaderT (mapLoggingT runExceptT)
  where
    errorHandler ::
      IOException ->
      ReaderT LspEnvironment (LoggingT IO) (Either IdeError a)
    errorHandler err = do
      logErrorN $ T.pack (show err)
      pure $ Left $ GeneralError $ show err

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
        res <- liftLspWithErr do
          initFinished
          logDebugN "Init finished"
        case res of
          Left err -> do
            liftLsp $ logErrorN $ "Initalise error: " <> show err
            sendInfoMsg "Failed to initialise lsp server"
          Right _ -> sendInfoMsg "OA purs lsp server initialized",
      Server.notificationHandler Message.SMethod_TextDocumentDidOpen $ \msg -> do
        liftLsp $ logDebugN "TextDocumentDidOpen"
        let uri :: Uri
            uri = getMsgUri msg
            fileName = Types.uriToFilePath uri
        void $ liftLspWithErr do
          waitForInit
          logDebugN "Rebuilding file from open"
          traverse rebuildFile fileName,
      Server.notificationHandler Message.SMethod_TextDocumentDidChange $ \msg -> do
        liftLsp $ logDebugN "TextDocumentDidChange",
      Server.notificationHandler Message.SMethod_TextDocumentDidSave $ \msg -> do
        liftLsp $ logDebugN "SMethod_TextDocumentDidSave"
        let uri :: Uri
            uri = getMsgUri msg
            fileName = Types.uriToFilePath uri
        void $ liftLspWithErr do
          waitForInit
          logDebugN "Rebuilding file from save"
          traverse rebuildFile fileName,
      Server.notificationHandler Message.SMethod_WorkspaceDidChangeConfiguration $ \msg -> do
        cfg <- getConfig
        liftLsp $ logDebugN $ "Config changed: " <> show cfg,
      Server.notificationHandler Message.SMethod_SetTrace $ \msg -> do
        liftLsp $ logDebugN "SMethod_SetTrace",
      Server.requestHandler Message.SMethod_TextDocumentDiagnostic $ \req res -> do
        liftLsp $ logDebugN "SMethod_TextDocumentDiagnostic"
        (errs, diagnostics) <- getFileDiagnotics req
        insertDiagnosticErrors diagErrs errs diagnostics
        res $
          Right $
            Types.DocumentDiagnosticReport $
              Types.InL $
                Types.RelatedFullDocumentDiagnosticReport Types.AString Nothing diagnostics Nothing,
      Server.requestHandler Message.SMethod_TextDocumentCodeAction $ \req res -> do
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
        liftLsp $ logDebugN "SMethod_TextDocumentHover"
        let Types.HoverParams docIdent pos@(Types.Position line col) _workDone = req ^. LSP.params
            filePathMb = Types.uriToFilePath $ docIdent ^. LSP.uri
            docUri =
              docIdent
                ^. LSP.uri
                  . to Types.toNormalizedUri
            nullRes = res $ Right $ Types.InR Types.Null

            markdownRes :: Text -> HandlerM () ()
            markdownRes md = res $ Right $ Types.InL $ Types.Hover (Types.InL $ Types.MarkupContent Types.MarkupKind_Markdown md) Nothing

            markdownTypeRes :: Text -> Maybe Text -> [P.Comment] -> HandlerM () ()
            markdownTypeRes word type' comments =
              markdownRes $ pursTypeStr word type' comments

            pursTypeStr word type' comments =
              "```purescript\n"
                <> word
                <> annotation
                <> "\n"
                <> fold (convertComments comments)
                <> "\n```"
              where
                annotation = case type' of
                  Just t -> " :: " <> t
                  Nothing -> ""

            forLsp :: Maybe a -> (a -> HandlerM () ()) -> HandlerM () ()
            forLsp val f = maybe nullRes f val

        forLsp filePathMb \filePath -> do
          corefnExprMb <- liftLsp $ getCoreFnExprAt filePath pos
          liftLsp $ logDebugN $ "Corefn expr: " <> show corefnExprMb
          forLsp corefnExprMb \case
            CF.Literal _ _ -> nullRes
            CF.Constructor (_ss, comments, meta) tName cMame _ -> do
              markdownTypeRes (P.runProperName cMame) (Just $ P.runProperName tName) comments
            CF.Var (_ss, comments, meta) (P.Qualified qb ident) -> do
              case qb of
                P.ByModuleName mName -> do
                  docsMb <- liftLsp $ readDeclarationDocsAsMarkdown mName (P.runIdent ident)
                  case docsMb of
                    Just docs -> markdownRes docs
                    _ -> do
                      declMb <- liftLsp $ getEfDeclarationInModule mName (runIdent ident)
                      markdownTypeRes (P.runIdent ident) (prettyPrintTypeSingleLine . efDeclSourceType <$> declMb) comments
                P.BySourcePos pos' ->
                  markdownTypeRes (P.runIdent ident) Nothing []
            _ -> do
              vfMb <- Server.getVirtualFile docUri
              forLsp vfMb \vf -> do
                let word = getWordAt (VFS._file_text vf) pos
                mNameMb <- liftLsp $ selectExternModuleNameFromFilePath filePath
                forLsp mNameMb \mName -> do
                  names <- liftLsp $ getNamesAtPosition pos mName (VFS._file_text vf)
                  liftLsp $ logDebugN $ "Names at position: " <> show (Set.toList names)
                  forLsp (head names) \name -> do
                    typeMb <- liftLsp $ lookupTypeInEnv name
                    liftLsp $ logDebugN $ "Type in env: " <> show typeMb
                    forLsp typeMb \t -> do
                      markdownTypeRes (printName $ disqualify name) (Just $ prettyPrintTypeSingleLine t) [],
      --   declMb <- liftLsp $ getEfDeclarationInModule mName name
      --   markdownTypeRes name (prettyPrintTypeSingleLine . efDeclSourceType <$> declMb) []
      -- -- pure (),
      Server.requestHandler Message.SMethod_TextDocumentDefinition $ \req res -> do
        sendInfoMsg "SMethod_TextDocumentDefinition"
        let Types.DefinitionParams docIdent pos _prog _prog' = req ^. LSP.params
            filePathMb = Types.uriToFilePath $ docIdent ^. LSP.uri
            uri :: Types.NormalizedUri
            uri =
              req
                ^. LSP.params
                  . LSP.textDocument
                  . LSP.uri
                  . to Types.toNormalizedUri

            nullRes = res $ Right $ Types.InR $ Types.InR Types.Null

            locationRes fp range = res $ Right $ Types.InL $ Types.Definition $ Types.InL $ Types.Location (Types.filePathToUri fp) range

            forLsp :: Maybe a -> (a -> HandlerM () ()) -> HandlerM () ()
            forLsp val f = maybe nullRes f val

        forLsp filePathMb \filePath -> do
          corefnExprMb <- liftLsp $ getCoreFnExprAt filePath pos
          forLsp corefnExprMb \case
            CF.Var (_ss, _comments, _meta) (P.Qualified qb ident) -> do
              let name = P.runIdent ident
              case qb of
                P.ByModuleName mName -> do
                  declMb <- liftLsp $ getEfDeclarationInModule mName name
                  forLsp declMb \decl -> do
                    modFpMb <- liftLsp $ selectExternPathFromModuleName mName
                    forLsp modFpMb \modFp -> do
                      let sourceSpan = efDeclSourceSpan decl
                      locationRes modFp (spanToRange sourceSpan)
                P.BySourcePos srcPos ->
                  locationRes filePath (Types.Range (sourcePosToPosition srcPos) (sourcePosToPosition srcPos))
            _ -> nullRes

            --   vfMb <- Server.getVirtualFile uri

            --   for_ vfMb \vf -> do
            --     let word = getWordAt (VFS._file_text vf) pos
            --     cache <- liftLsp cachedRebuild
            --     let moduleName' = case cache of
            --           Right (Just (mName, _)) -> Just mName
            --           _ -> Nothing

            --     imports <-
            --       filePathMb
            --         & maybe (pure Nothing) (fmap hush . liftLsp . parseImportsFromFile)

            --     let filters :: [Filter]
            --         filters =
            --           imports
            --             & maybe [] (pure . (moduleFilter . insertCurrentModule . Set.fromList . fmap getInputModName) . snd)

            --         getInputModName (n, _, _) = n

            --         insertCurrentModule :: Set P.ModuleName -> Set P.ModuleName
            --         insertCurrentModule mods = maybe mods (flip Set.insert mods) moduleName'

            --     completions :: Either IdeError [Completion] <- liftLsp $ getExactCompletionsWithPrim word filters moduleName'

            --     sendInfoMsg $ "Completions: " <> show completions
            --     let withLocation =
            --           fold completions
            --             & mapMaybe
            --               ( \c -> case complLocation c of
            --                   Just loc -> Just (c, loc)
            --                   Nothing -> Nothing
            --               )
            --             & head

            --     paths <- liftLsp $ Map.map snd . fsModules <$> getFileState

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
          res <- fmap snd <$> liftLspWithErr (waitForInit *> logWarnN "rebuilding for diagnostics" *> rebuildFile file)
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
        liftLsp $ logErrorN $ "Rebuild error: " <> textError err
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
spanToRange (Errors.SourceSpan _ start end) =
  Types.Range
    (sourcePosToPosition start)
    (sourcePosToPosition end)


sendError :: IdeError -> HandlerM config ()
sendError err =
  Server.sendNotification
    Message.SMethod_WindowShowMessage
    ( Types.ShowMessageParams Types.MessageType_Error $
        "Something went wrong:\n" <> textError err
    )


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
            ( runErrLogger LogAll
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
