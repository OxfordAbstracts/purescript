module Language.PureScript.Lsp.Imports where

import Control.Lens (set)
import Control.Monad.Catch (MonadThrow)
import Data.Maybe as Maybe
import Data.Text qualified as T
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Types as LSP
import Language.PureScript.AST.Declarations qualified as P
import Language.PureScript.AST.SourcePos (nullSourceSpan)
import Language.PureScript.Ide.Imports (Import (Import), prettyPrintImportSection, sliceImportSection)
import Language.PureScript.Lsp.Log (errorLsp)
import Language.PureScript.Lsp.ReadFile (lspReadFile)
import Language.PureScript.Lsp.Types (CompleteItemData (..), LspEnvironment)
import Language.PureScript.Names qualified as P
import Protolude
import Language.PureScript.Lsp.Cache.Query (getAstDeclarationInModule)

addImportToTextEdit :: (MonadIO m, MonadReader LspEnvironment m, MonadThrow m) => CompletionItem -> CompleteItemData -> m CompletionItem
addImportToTextEdit completionItem completeItemData = do
  importEdits <- getImportEdits completeItemData
  pure $ set LSP.additionalTextEdits importEdits completionItem

getImportEdits :: (MonadIO m, MonadReader LspEnvironment m, MonadThrow m) => CompleteItemData -> m (Maybe [TextEdit])
getImportEdits (CompleteItemData path moduleName' importedModuleName name _word) = do
  parseRes <- parseImportsFromFile path
  case parseRes of
    Left err -> do
      errorLsp $ "In " <> T.pack path <> " failed to parse imports from file: " <> err
      pure Nothing
    Right (_mn, before, imports, _after) -> do
      declMb <- getAstDeclarationInModule importedModuleName name
      case declMb of
        Nothing -> do
          errorLsp $ "In " <> T.pack path <> " failed to get declaration from module: " <> name
          pure Nothing
        Just decl -> do
          addDeclarationToImports moduleName' importedModuleName decl imports
            <&> pure . importsToTextEdit before
            & pure

addDeclarationToImports :: P.ModuleName -> P.ModuleName -> P.Declaration -> [Import] -> Maybe [Import]
addDeclarationToImports moduleName' importedModuleName decl imports
  | importingSelf = Nothing
  | Just existing <- alreadyImportedModuleMb,
    Just ref <- refMb = case existing of
      Import _ (P.Explicit refs') _ ->
        if ref `notElem` refs'
          then Just $ Import importedModuleName (P.Explicit (refs' <> [ref])) Nothing : withoutOldImport
          else Nothing
      Import _ P.Implicit _ -> Nothing
      Import _ (P.Hiding refs') _ ->
        if ref `elem` refs'
          then Just $ Import importedModuleName (P.Hiding (filter (/= ref) refs')) Nothing : withoutOldImport
          else Nothing
  | otherwise = Just $ Import importedModuleName (P.Explicit refs) Nothing : imports
  where
    withoutOldImport :: [Import]
    withoutOldImport =  maybe identity (\im -> filter (/= im)) alreadyImportedModuleMb imports  
      
    refs :: [P.DeclarationRef]
    refs = toList refMb

    refMb :: Maybe P.DeclarationRef
    refMb =
      P.declName decl >>= \case
        P.IdentName name -> Just $ P.ValueRef nullSourceSpan name
        P.ValOpName name -> Just $ P.ValueOpRef nullSourceSpan name
        P.TyName name -> Just $ P.TypeRef nullSourceSpan name Nothing
        P.TyOpName name -> Just $ P.TypeOpRef nullSourceSpan name
        P.TyClassName name -> Just $ P.TypeClassRef nullSourceSpan name
        P.ModName name -> Just $ P.ModuleRef nullSourceSpan name
        P.DctorName _name -> Nothing

    alreadyImportedModuleMb =
      find (\(Import mn' _ _) -> mn' == importedModuleName) imports

    importingSelf = moduleName' == importedModuleName

importsToTextEdit :: [Text] -> [Import] -> TextEdit
importsToTextEdit before imports =
  TextEdit
    ( LSP.Range
        (LSP.Position beforeLine 0)
        (LSP.Position (beforeLine + fromIntegral (length printed)) 0)
    )
    (T.unlines printed)
  where
    beforeLine = fromIntegral $ length before
    printed = prettyPrintImportSection imports

-- | Reads a file and returns the (lines before the imports, the imports, the
-- lines after the imports)
parseImportsFromFile ::
  (MonadIO m, MonadThrow m) =>
  FilePath ->
  m (Either Text (P.ModuleName, [Text], [Import], [Text]))
parseImportsFromFile fp = do
  (_, file) <- lspReadFile fp
  pure $ sliceImportSection (T.lines file)
