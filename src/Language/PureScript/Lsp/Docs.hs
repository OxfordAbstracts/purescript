module Language.PureScript.Lsp.Docs where

import Control.Arrow ((>>>))
import Language.LSP.Server (MonadLsp, getConfig)
import Language.PureScript.AST.SourcePos qualified as P
import Language.PureScript.Docs qualified as Docs
import Language.PureScript.Docs.AsMarkdown (declAsMarkdown, runDocs)
import Language.PureScript.Docs.Collect (parseDocsJsonFile)
import Language.PureScript.Docs.Types (Declaration (declChildren))
import Language.PureScript.Docs.Types qualified as P
import Language.PureScript.Lsp.NameType (LspNameType (..))
import Language.PureScript.Lsp.Print (printName)
import Language.PureScript.Lsp.ServerConfig (ServerConfig (outputPath))
import Language.PureScript.Lsp.Types (LspEnvironment)
import Language.PureScript.Names qualified as P
import Protolude

readModuleDocs :: (MonadLsp ServerConfig m) => P.ModuleName -> m (Maybe Docs.Module)
readModuleDocs modName = do
  outputDirectory <- outputPath <$> getConfig
  liftIO $ catchError (Just <$> parseDocsJsonFile outputDirectory modName) (const $ pure Nothing)

readDeclarationDocs :: (MonadLsp ServerConfig m) => P.ModuleName -> Text -> m (Maybe Docs.Declaration)
readDeclarationDocs modName ident = do
  modMb <- readModuleDocs modName
  pure $ modMb >>= (P.modDeclarations >>> find ((== ident) . P.declTitle))

-- todo: add child info and operator matching
readDeclarationDocsWithNameType :: (MonadReader LspEnvironment m, MonadLsp ServerConfig m) => P.ModuleName -> LspNameType -> Text -> m (Maybe Text)
readDeclarationDocsWithNameType modName nameType ident = do
  modMb <- readModuleDocs modName
  pure $ modMb >>= (P.modDeclarations >>> getMarkdown)
  where
    getMarkdown :: [Docs.Declaration] -> Maybe Text
    getMarkdown [] = Nothing
    getMarkdown (decl : decls) = case decl of
      _ | matchesNameType decl -> Just $ runDocs $ declAsMarkdown decl
      _ | matchesChildren (declChildren decl) -> Just $ runDocs $ declAsMarkdown decl
      _ -> getMarkdown decls

    matchesNameType :: P.Declaration -> Bool
    matchesNameType d = case P.declInfo d of
      P.ValueDeclaration _ -> nameType == IdentNameType && P.declTitle d == ident
      P.DataDeclaration _ _ _ -> nameType == TyNameType && P.declTitle d == ident
      P.TypeSynonymDeclaration _ _ -> nameType == TyNameType && P.declTitle d == ident
      P.TypeClassDeclaration _ _ _ -> nameType == TyClassNameType && P.declTitle d == ident
      _ -> False

    matchesChildren :: [P.ChildDeclaration] -> Bool
    matchesChildren = any matchesChild

    matchesChild :: P.ChildDeclaration -> Bool
    matchesChild cd = case P.cdeclInfo cd of
      P.ChildInstance _ _ -> nameType == TyClassNameType && P.cdeclTitle cd == ident
      P.ChildDataConstructor _ -> nameType == DctorNameType && P.cdeclTitle cd == ident
      P.ChildTypeClassMember _ -> nameType == IdentNameType && P.cdeclTitle cd == ident

readDeclarationDocsAsMarkdown :: (MonadReader LspEnvironment m, MonadLsp ServerConfig m) => P.ModuleName -> Text -> m (Maybe Text)
readDeclarationDocsAsMarkdown modName ident = fmap (runDocs . declAsMarkdown) <$> readDeclarationDocs modName ident

readQualifiedNameDocsAsMarkdown :: (MonadReader LspEnvironment m, MonadLsp ServerConfig m) => P.Qualified P.Name -> m (Maybe Text)
readQualifiedNameDocsAsMarkdown = \case
  (P.Qualified (P.ByModuleName modName) ident) -> readDeclarationDocsAsMarkdown modName (printName ident)
  _ -> pure Nothing

readDeclarationDocsSourceSpan :: (MonadReader LspEnvironment m, MonadLsp ServerConfig m) => P.ModuleName -> Text -> m (Maybe P.SourceSpan)
readDeclarationDocsSourceSpan modName ident = readDeclarationDocs modName ident <&> (=<<) P.declSourceSpan

readQualifiedNameDocsSourceSpan :: (MonadReader LspEnvironment m, MonadLsp ServerConfig m) => P.Qualified P.Name -> m (Maybe P.SourceSpan)
readQualifiedNameDocsSourceSpan = \case
  (P.Qualified (P.ByModuleName modName) ident) -> readDeclarationDocsSourceSpan modName (printName ident)
  _ -> pure Nothing