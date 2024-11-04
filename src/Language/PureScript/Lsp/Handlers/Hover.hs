{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.PureScript.Lsp.Handlers.Hover (hoverHandler) where

import Control.Arrow ((>>>))
import Control.Lens (Field2 (_2), Field3 (_3), (^.))
import Control.Lens.Combinators (view)
import Control.Monad.Trans.Writer (WriterT (runWriterT))
import Data.Map qualified as M
import Data.Text qualified as T
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as Message
import Language.LSP.Protocol.Types qualified as Types
import Language.LSP.Server qualified as Server
import Language.PureScript qualified as P
import Language.PureScript.Docs.Convert.Single (convertComments)
import Language.PureScript.Docs.Types qualified as Docs
import Language.PureScript.Errors (Literal (..))
import Language.PureScript.Ide.Error (prettyPrintTypeSingleLine)
import Language.PureScript.Lsp.AtPosition (EverythingAtPos (..), binderSourceSpan, getEverythingAtPos, getImportRefNameType, modifySmallestBinderAtPos, modifySmallestExprAtPos, showCounts, smallestExpr', smallestType, spanToRange, debugExpr)
import Language.PureScript.Lsp.Cache (selectDependencies)
import Language.PureScript.Lsp.Cache.Query (getAstDeclarationTypeInModule)
import Language.PureScript.Lsp.Docs (readDeclarationDocsWithNameType, readModuleDocs)
import Language.PureScript.Lsp.Log (debugLsp)
import Language.PureScript.Lsp.Monad (HandlerM)
import Language.PureScript.Lsp.NameType (LspNameType (..))
import Language.PureScript.Lsp.Print (printName)
import Language.PureScript.Lsp.Rebuild (buildExportEnvCacheAndHandleErrors)
import Language.PureScript.Lsp.ServerConfig (getInferExpressions)
import Language.PureScript.Lsp.State (cachedRebuild, getExportEnv)
import Language.PureScript.Lsp.Types (ExternDependency (edExtern), OpenFile (..))
import Language.PureScript.Lsp.Util (positionToSourcePos)
import Language.PureScript.Sugar.Names.Env qualified as P
import Language.PureScript.TypeChecker.IdeArtifacts (IdeArtifact (..), IdeArtifactValue (..), getArtifactsAtPosition, smallestArtifact, debugIdeArtifact)
import Protolude hiding (handle, to)
import Text.PrettyPrint.Boxes (render)

hoverHandler :: Server.Handlers HandlerM
hoverHandler = Server.requestHandler Message.SMethod_TextDocumentHover $ \req res -> do
  let Types.HoverParams docIdent startPos _prog = req ^. LSP.params
      filePathMb = Types.uriToFilePath $ docIdent ^. LSP.uri

      nullRes = res $ Right $ Types.InR Types.Null

      markdownRes range md =
        res $ Right $ Types.InL $ Types.Hover (Types.InL $ Types.MarkupContent Types.MarkupKind_Markdown md) range

      forLsp :: Maybe a -> (a -> HandlerM ()) -> HandlerM ()
      forLsp val f = maybe nullRes f val

      respondWithDeclInModule :: P.SourceSpan -> LspNameType -> P.ModuleName -> Text -> HandlerM ()
      respondWithDeclInModule ss nameType modName ident = do
        docs <- readDeclarationDocsWithNameType modName nameType ident
        foundTypes <- getAstDeclarationTypeInModule (Just nameType) modName ident
        debugLsp $ "respondWithDeclInModule " <> show (modName, ident)
        debugLsp $ "Found types: " <> show foundTypes
        debugLsp $ "Found docs: " <> show (isJust docs)
        markdownRes (Just $ spanToRange ss) $
          joinMarkup
            [ showTypeSection modName ident <$> head foundTypes,
              showDocs <$> docs
            ]

      respondWithModule :: P.SourceSpan -> P.ModuleName -> HandlerM ()
      respondWithModule ss modName = do
        docsMb <- readModuleDocs modName
        case docsMb of
          Just docs | Just comments <- Docs.modComments docs -> markdownRes (Just $ spanToRange ss) comments
          _ -> nullRes

      respondWithImport :: P.SourceSpan -> P.ModuleName -> Maybe P.DeclarationRef -> HandlerM ()
      respondWithImport ss importedModuleName (Just ref) = do
        let name = P.declRefName ref
            nameType = getImportRefNameType ref
        respondWithDeclInModule ss nameType importedModuleName (printName name)
      respondWithImport ss importedModuleName _ = respondWithModule ss importedModuleName

      handleLiteral :: P.SourceSpan -> P.Literal a -> HandlerM ()
      handleLiteral ss = \case
        P.NumericLiteral (Left int) -> do
          markdownRes (Just $ spanToRange ss) (pursTypeStr (show int) (Just "Int") [])
        P.NumericLiteral (Right n) -> do
          markdownRes (Just $ spanToRange ss) (pursTypeStr (show n) (Just "Number") [])
        P.StringLiteral str -> do
          markdownRes (Just $ spanToRange ss) (pursTypeStr (ellipsis 64 $ show str) (Just "String") [])
        P.CharLiteral ch -> do
          markdownRes (Just $ spanToRange ss) (pursTypeStr (show ch) (Just "Char") [])
        P.BooleanLiteral b -> do
          markdownRes (Just $ spanToRange ss) (pursTypeStr (show b) (Just "Boolean") [])
        -- should not be reachable
        _ -> nullRes

      lookupExprTypes :: P.Expr -> HandlerM [Text]
      lookupExprTypes = \case
        P.Var _ (P.Qualified (P.ByModuleName modName) ident) -> do
          fmap (showTypeSection modName (P.runIdent ident)) <$> getAstDeclarationTypeInModule (Just IdentNameType) modName (P.runIdent ident)
        P.Op _ (P.Qualified (P.ByModuleName modName) op) -> do
          fmap (showTypeSection modName (P.runOpName op)) <$> getAstDeclarationTypeInModule (Just ValOpNameType) modName (P.runOpName op)
        P.Constructor _ (P.Qualified (P.ByModuleName modName) dctor) -> do
          fmap (showTypeSection modName (P.runProperName dctor)) <$> getAstDeclarationTypeInModule (Just DctorNameType) modName (P.runProperName dctor)
        P.TypedValue _ e _ | not (generatedExpr e) -> do
          lookupExprTypes e
        P.PositionedValue _ _ e | not (generatedExpr e) -> do
          lookupExprTypes e
        _ -> pure []

      lookupExprDocs :: P.Expr -> HandlerM (Maybe Text)
      lookupExprDocs = \case
        P.Var _ (P.Qualified (P.ByModuleName modName) ident) -> do
          readDeclarationDocsWithNameType modName IdentNameType (P.runIdent ident)
        P.Op _ (P.Qualified (P.ByModuleName modName) op) -> do
          readDeclarationDocsWithNameType modName ValOpNameType (P.runOpName op)
        P.Constructor _ (P.Qualified (P.ByModuleName modName) dctor) -> do
          readDeclarationDocsWithNameType modName DctorNameType (P.runProperName dctor)
        _ -> pure Nothing
  debugLsp $ "Hover request for: " <> show filePathMb
  forLsp filePathMb \filePath -> do
    cacheOpenMb <- cachedRebuild filePath
    debugLsp $ "Cache found: " <> show (isJust cacheOpenMb)
    forLsp cacheOpenMb \OpenFile {..} -> do
      let everything = getEverythingAtPos (P.getModuleDeclarations ofModule) startPos
          respondWithCounts = markdownRes Nothing $ showCounts everything
          atPos = getArtifactsAtPosition (positionToSourcePos startPos) (P.checkIdeArtifacts ofEndCheckState)
      -- debugLsp $ showCounts everything
      debugLsp $ "at pos len: " <> show (length atPos)
      debugLsp $ "smallest: " <> (ellipsis 512 . show) (debugExpr . iaValue <$> smallestArtifact atPos)
      for_ atPos \a -> do 
        debugLsp $ debugIdeArtifact a
        case iaValue a of 
          IaExpr label e -> debugLsp $ "Expr: " <> label <> "\n" <>  (ellipsis 256 $ debugExpr e)
          _ -> pure ()
      case smallestArtifact atPos of
        Just (IdeArtifact {..}) ->
          case iaValue of
            IaExpr _ expr -> do
              let inferredRes = pursTypeStr (dispayExprOnHover expr) (Just $ prettyPrintTypeSingleLine iaType) []
              foundTypes <- lookupExprTypes expr
              docs <- lookupExprDocs expr
              markdownRes (Just $ spanToRange iaSpan) $
                joinMarkup
                  [ Just inferredRes,
                    head foundTypes,
                    showDocs <$> docs
                  ]
            IaTypeName name -> do
              let name' = P.runProperName name
                  inferredRes = pursTypeStr name' (Just $ prettyPrintTypeSingleLine iaType) []
                  modName = fromMaybe ofModuleName iaDefinitionModule
              docs <- readDeclarationDocsWithNameType modName TyNameType name'
              foundTypes <- getAstDeclarationTypeInModule (Just TyNameType) modName name'
              markdownRes (Just $ spanToRange iaSpan) $
                joinMarkup
                  [ Just inferredRes,
                    showTypeSection modName (P.runProperName name) <$> head foundTypes,
                    showDocs <$> docs
                  ]
            IaClassName name -> do
              let name' = P.runProperName name
                  inferredRes = pursTypeStr name' (Just $ prettyPrintTypeSingleLine iaType) []
                  modName = fromMaybe ofModuleName iaDefinitionModule
              docs <- readDeclarationDocsWithNameType modName TyClassNameType name'
              foundTypes <- getAstDeclarationTypeInModule (Just TyClassNameType) modName name'
              markdownRes (Just $ spanToRange iaSpan) $
                joinMarkup
                  [ Just inferredRes,
                    showTypeSection modName (P.runProperName name) <$> head foundTypes,
                    showDocs <$> docs
                  ]
            IaIdent ident -> do
              markdownRes (Just $ spanToRange iaSpan) $ pursTypeStr ident (Just $ prettyPrintTypeSingleLine iaType) []
            IaBinder binder -> do
              let inferredRes = pursTypeStr (dispayBinderOnHover binder) (Just $ prettyPrintTypeSingleLine iaType) []
              markdownRes (spanToRange <$> binderSourceSpan binder) inferredRes
            IaDecl decl -> do
              markdownRes (Just $ spanToRange iaSpan) $ pursTypeStr (maybe "_" printName $ P.declName decl) (Just $ prettyPrintTypeSingleLine iaType) []
            IaType ty -> do
              markdownRes (Just $ spanToRange iaSpan) $ pursTypeStr (prettyPrintTypeSingleLine ty) (Just $ prettyPrintTypeSingleLine iaType) []
        _ ->
          case head $ apImport everything of
            Just (ss, importedModuleName, _, ref) -> do
              respondWithImport ss importedModuleName ref
            _ -> do
              case smallestExpr' (view _3) $ filter (not . (isAbs <||> generatedExpr) . view _3) $ apExprs everything of
                Just (_, _, P.Literal ss literal) | isLiteralNode literal -> handleLiteral ss literal
                Just (ss, _, foundExpr) -> do
                  inferredRes <- inferExprViaTypeHoleText filePath startPos
                  foundTypes <- lookupExprTypes foundExpr
                  docs <- lookupExprDocs foundExpr
                  markdownRes (Just $ spanToRange ss) $
                    joinMarkup
                      [ inferredRes <|> Just (dispayExprOnHover foundExpr),
                        head foundTypes,
                        showDocs <$> docs
                      ]
                Nothing -> do
                  binderInferredRes <- inferBinderViaTypeHole filePath startPos
                  case binderInferredRes of
                    Just (binder, ty) -> do
                      debugLsp $ "Found binder: " <> show binder
                      markdownRes
                        (spanToRange <$> binderSourceSpan binder)
                        (pursTypeStr (dispayBinderOnHover binder) (Just $ prettyPrintTypeSingleLine ty) [])
                    Nothing -> do
                      case smallestType $ apTypes everything of
                        Just (P.ConstrainedType ann (P.Constraint _ (P.Qualified (P.ByModuleName mName) ident) _ _ _) _) -> do
                          debugLsp $ "Found constrained type: " <> show ident
                          respondWithDeclInModule (fst ann) TyClassNameType mName $ P.runProperName ident
                        Just (P.TypeConstructor ann (P.Qualified (P.ByModuleName mName) name)) | P.runProperName name /= "Type" -> do
                          debugLsp $ "Found type constructor: " <> show name
                          respondWithDeclInModule (fst ann) TyNameType mName (P.runProperName name)
                        Just (P.TypeOp ann (P.Qualified (P.ByModuleName mName) name)) -> do
                          debugLsp $ "Found type op: " <> show name
                          respondWithDeclInModule (fst ann) TyOpNameType mName (P.runOpName name)
                        _ -> do
                          debugLsp "Looking for unsugared types"
                          let typesBeforeSugaring = apTypes $ getEverythingAtPos (P.getModuleDeclarations ofUncheckedModule) startPos
                          case smallestType typesBeforeSugaring of
                            Just ty -> do
                              exportEnv <- getExportEnv
                              let imports = maybe P.nullImports (view _2) $ M.lookup ofModuleName exportEnv

                              case ty of
                                P.TypeConstructor ann q@(P.Qualified _ name) | P.runProperName name /= "Type" -> do
                                  let mName = fromMaybe ofModuleName $ fmap P.importSourceModule . head =<< M.lookup q (P.importedTypes imports)
                                  respondWithDeclInModule (fst ann) TyNameType mName (P.runProperName name)
                                P.ConstrainedType ann (P.Constraint _ q@(P.Qualified _ ident) _ _ _) _ -> do
                                  let mName = fromMaybe ofModuleName $ fmap P.importSourceModule . head =<< M.lookup q (P.importedTypeClasses imports)
                                  respondWithDeclInModule (fst ann) TyClassNameType mName $ P.runProperName ident
                                _ -> do
                                  markdownRes
                                    (Just $ spanToRange $ fst $ P.getAnnForType ty)
                                    (pursTypeStr (prettyPrintTypeSingleLine ty) Nothing [])
                            Nothing -> do
                              respondWithCounts

showTypeSection :: P.ModuleName -> Text -> Text -> Text
showTypeSection mName expr ty = "*" <> P.runModuleName mName <> "*\n" <> pursMd (expr <> " :: " <> ty)

showDocs :: Text -> Text
showDocs d = "**Docs**\n" <> d

isLiteralNode :: Literal P.Expr -> Bool
isLiteralNode = \case
  NumericLiteral _ -> True
  StringLiteral _ -> True
  CharLiteral _ -> True
  BooleanLiteral _ -> True
  _ -> False

joinMarkup :: [Maybe Text] -> Text
joinMarkup = T.intercalate "\n---\n" . catMaybes

inferExprViaTypeHoleText :: FilePath -> Types.Position -> HandlerM (Maybe Text)
inferExprViaTypeHoleText filePath pos =
  inferExprViaTypeHole filePath pos <&> fmap \(expr, t) ->
    pursTypeStr (dispayExprOnHover expr) (Just $ prettyPrintTypeSingleLine t) []

inferExprViaTypeHole :: FilePath -> Types.Position -> HandlerM (Maybe (P.Expr, P.SourceType))
inferExprViaTypeHole = inferViaTypeHole (modifySmallestExprAtPos addExprTypeHoleAnnotation)

inferBinderViaTypeHole :: FilePath -> Types.Position -> HandlerM (Maybe (P.Binder, P.SourceType))
inferBinderViaTypeHole = inferViaTypeHole (modifySmallestBinderAtPos addBinderTypeHoleAnnotation)

inferViaTypeHole ::
  ( Types.Position ->
    P.Module ->
    (P.Module, Maybe (a, a))
  ) ->
  FilePath ->
  Types.Position ->
  HandlerM (Maybe (a, P.SourceType))
inferViaTypeHole addHole filePath pos = do
  shouldInferUsingTypeHole <- getInferExpressions
  if not shouldInferUsingTypeHole
    then pure Nothing
    else do
      cacheOpenMb <- cachedRebuild filePath
      cacheOpenMb & maybe (pure Nothing) \OpenFile {..} -> do
        let module' = P.importPrim ofUncheckedModule
            (moduleWithHole, values) = addHole pos module'
        case values of
          Nothing -> pure Nothing
          Just (valueBefore, _valueAfter) -> do
            let externs = fmap edExtern ofDependencies
            (exportEnv, _) <- buildExportEnvCacheAndHandleErrors (selectDependencies module') module' externs
            (checkRes, warnings) <-
              runWriterT $
                runExceptT $
                  P.desugarAndTypeCheck P.emptyCheckState Nothing ofModuleName externs moduleWithHole exportEnv ofStartingEnv
            case checkRes of
              Right _ -> pure $ (valueBefore,) <$> findHoleType warnings
              Left errs -> do
                pure $
                  (valueBefore,) <$> findHoleType (warnings <> errs)

-- inferOnDeclViaTypeHole ::
--   ( Types.Position ->
--     P.Module ->
--     Maybe (P.Declaration, Maybe (a, a))
--   ) ->
--   FilePath ->
--   Types.Position ->
--   HandlerM (Maybe (a, P.SourceType))
-- inferOnDeclViaTypeHole addHole filePath pos = do
--   shouldInferUsingTypeHole <- getInferExpressions
--   if not shouldInferUsingTypeHole
--     then pure Nothing
--     else do
--       cacheOpenMb <- cachedRebuild filePath
--       cacheOpenMb & maybe (pure Nothing) \OpenFile {..} -> do
--         let module' = P.importPrim ofUncheckedModule
--             withHole = addHole pos module'
--         case withHole of
--           Just (declWithHole, Just (valueBefore, _valueAfter)) -> do
--             let externs = fmap edExtern ofDependencies
--             (exEnv, _) <- buildExportEnvCacheAndHandleErrors (selectDependencies module') module' externs
--             (checkRes, warnings) <-
--               runWriterT $
--                 runExceptT $
--                   evalSupplyT 0 $
--                     evalStateT
--                       (typeCheckDecl (view _3 <$> exEnv) ofModuleName (P.getModuleDeclarations module') declWithHole)
--                       ((P.emptyCheckState $ removeDeclFromEnv ofModuleName declWithHole ofEndEnv) {P.checkCurrentModule = Just ofModuleName})

--             case checkRes of
--               Right _ -> do
--                 debugLsp "Decl hole error not found"
--                 pure $ (valueBefore,) <$> findHoleType warnings
--               Left errs -> do
--                 debugLsp $ "Errors: \n" <> T.pack (P.prettyPrintMultipleErrors P.noColorPPEOptions errs)
--                 pure $
--                   (valueBefore,) <$> findHoleType (warnings <> errs)
--           _ -> do
--             warnLsp "Decl with hole not found"
--             pure Nothing
--   where
--     typeCheckDecl modulesExports mn decls decl = do
--       let (_, imports) = partitionEithers $ fromImportDecl modulesExports <$> decls
--       modify (\s -> s {P.checkCurrentModule = Just mn, P.checkCurrentModuleImports = imports})
--       P.typeCheckAll mn [ignoreWildcardsUnderCompleteTypeSignatures decl]

--     fromImportDecl ::
--       M.Map P.ModuleName P.Exports ->
--       P.Declaration ->
--       Either
--         P.Declaration
--         ( P.SourceAnn,
--           P.ModuleName,
--           P.ImportDeclarationType,
--           Maybe P.ModuleName,
--           M.Map (P.ProperName 'P.TypeName) ([P.ProperName 'P.ConstructorName], P.ExportSource)
--         )
--     fromImportDecl modulesExports (P.ImportDeclaration sa moduleName' importDeclarationType asModuleName) =
--       Right (sa, moduleName', importDeclarationType, asModuleName, foldMap P.exportedTypes $ M.lookup moduleName' modulesExports)
--     fromImportDecl _ decl = Left decl

-- removeDeclFromEnv :: P.ModuleName -> P.Declaration -> P.Environment -> P.Environment
-- removeDeclFromEnv mName decl env = case decl of
--   P.ValueDecl _ ident _ _ _ -> env {E.names = M.delete (P.Qualified (P.ByModuleName mName) ident) (E.names env)}
--   _ -> env

findHoleType :: P.MultipleErrors -> Maybe P.SourceType
findHoleType = P.runMultipleErrors >>> findMap getHoverHoleType

getHoverHoleType :: P.ErrorMessage -> Maybe P.SourceType
getHoverHoleType =
  P.unwrapErrorMessage >>> \case
    P.HoleInferredType label t _ _ | label == hoverHoleLabel -> Just t
    _ -> Nothing

findMap :: (a -> Maybe b) -> [a] -> Maybe b
findMap f = listToMaybe . mapMaybe f

addExprTypeHoleAnnotation :: P.Expr -> P.Expr
addExprTypeHoleAnnotation expr = P.TypedValue False expr (P.TypeWildcard P.nullSourceAnn $ P.HoleWildcard hoverHoleLabel)

addBinderTypeHoleAnnotation :: P.Binder -> P.Binder
addBinderTypeHoleAnnotation b = P.ParensInBinder (P.TypedBinder (P.TypeWildcard P.nullSourceAnn $ P.HoleWildcard hoverHoleLabel) b) -- parens seems to be needed. For some desugaring reason?

hoverHoleLabel :: Text
hoverHoleLabel = "HOVER"

isAbs :: P.Expr -> Bool
isAbs = \case
  P.Abs _ _ -> True
  P.TypedValue _ e _ -> isAbs e
  _ -> False

generatedExpr :: P.Expr -> Bool
generatedExpr = \case
  P.Var _ ident -> generatedIdent $ P.disqualify ident
  P.Abs b e -> generatedBinder b || generatedExpr e
  P.App e e' -> generatedExpr e || generatedExpr e'
  P.TypedValue _ e _ -> generatedExpr e
  P.PositionedValue _ _ e -> generatedExpr e
  P.Case es _ -> any generatedExpr es
  _ -> False

generatedBinder :: P.Binder -> Bool
generatedBinder = \case
  P.VarBinder ss ident -> (ss == P.nullSourceSpan) || generatedIdent ident
  P.NamedBinder ss ident _ -> (ss == P.nullSourceSpan) || generatedIdent ident
  _ -> False

generatedIdent :: P.Ident -> Bool
generatedIdent = \case
  P.GenIdent {} -> True
  _ -> False

dispayExprOnHover :: P.Expr -> T.Text
dispayExprOnHover (P.Op _ (P.Qualified _ op)) = P.runOpName op -- Op's hit an infinite loop when pretty printed by themselves
dispayExprOnHover (P.Case _ _) = "<case expr>" -- case expressions are too large to pretty print in hover and are on mulitple lines
dispayExprOnHover expr = ellipsis 128 $ on1Line $ T.strip $ T.pack $ render $ P.prettyPrintValue 8 expr

dispayBinderOnHover :: P.Binder -> T.Text
dispayBinderOnHover binder = ellipsis 32 $ on1Line $ T.strip $ P.prettyPrintBinder binder

on1Line :: T.Text -> T.Text
on1Line = T.intercalate " " . T.lines

ellipsis :: Int -> Text -> Text
ellipsis l t = if T.length t > l then T.take l t <> "..." else t

pursTypeStr :: Text -> Maybe Text -> [P.Comment] -> Text
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

pursMd :: Text -> Text
pursMd t = "```purescript\n" <> t <> "\n```"

data InferError
  = FileNotCached
  | CompilationError P.MultipleErrors
  | InferException Text
  deriving (Show, Exception)

-- inferExprType :: FilePath -> P.Expr -> HandlerM (Either InferError P.SourceType)
-- inferExprType filePath expr = do
--   cacheOpenMb <- cachedRebuild filePath
--   case cacheOpenMb of
--     Nothing -> pure $ Left FileNotCached
--     Just OpenFile {..} -> do
--       inferRes <- runWriterT $ runExceptT $ evalSupplyT 0 $ evalStateT (infer' expr) ((P.emptyCheckState ofStartingEnv) {P.checkCurrentModule = Just ofModuleName})
--       pure $ bimap CompilationError (\(P.TypedValue' _ _ t) -> t) $ fst inferRes

-- inferExprType' :: FilePath -> P.Expr -> HandlerM P.SourceType
-- inferExprType' fp =
--   inferExprType fp >=> \case
--     Right t -> pure t
--     Left e -> throwIO e
