module Language.PureScript.Lsp.Docs where

import Control.Arrow ((>>>))
import Language.PureScript.Docs qualified as Docs
import Language.PureScript.Docs.AsMarkdown (declAsMarkdown, runDocs)
import Language.PureScript.Docs.Collect (parseDocsJsonFile)
import Language.PureScript.Docs.Types qualified as P
import Language.PureScript.Lsp.Print (printName)
import Language.PureScript.Lsp.Types (LspConfig (confOutputPath), LspEnvironment (lspConfig))
import Language.PureScript.Names qualified as P
import Protolude

readDeclarationDocs :: (MonadIO m, MonadReader LspEnvironment m) => P.ModuleName -> Text -> m (Maybe Docs.Declaration)
readDeclarationDocs modName ident = do
  outputDirectory <- asks (confOutputPath . lspConfig)
  modMb <- liftIO $ catchError (Just <$> parseDocsJsonFile outputDirectory modName) (const $ pure Nothing)
  pure $ modMb >>= (P.modDeclarations >>> find ((== ident) . P.declTitle))

readDeclarationDocsAsMarkdown :: (MonadIO m, MonadReader LspEnvironment m) => P.ModuleName -> Text -> m (Maybe Text)
readDeclarationDocsAsMarkdown modName ident = fmap (runDocs . declAsMarkdown) <$> readDeclarationDocs modName ident

readQualifiedNameDocsAsMarkdown :: (MonadIO m, MonadReader LspEnvironment m) => P.Qualified P.Name -> m (Maybe Text)
readQualifiedNameDocsAsMarkdown = \case
  (P.Qualified (P.ByModuleName modName) ident) -> readDeclarationDocsAsMarkdown modName (printName ident)
  _ -> pure Nothing