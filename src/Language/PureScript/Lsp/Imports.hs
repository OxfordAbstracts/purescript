module Language.PureScript.Lsp.Imports where

import Control.Lens (set)
import Control.Monad.Catch (MonadThrow)
import Data.List (init, last)
import Data.Maybe as Maybe
import Data.Text qualified as T
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Types as LSP
import Language.PureScript.AST.Declarations qualified as P
import Language.PureScript.AST.SourcePos (nullSourceSpan)
import Language.PureScript.Ide.Imports (Import (Import), prettyPrintImportSection, sliceImportSection)
import Language.PureScript.Lsp.Cache.Query (getAstDeclarationInModule)
import Language.PureScript.Lsp.Log (errorLsp)
import Language.PureScript.Lsp.ReadFile (lspReadFile)
import Language.PureScript.Lsp.Types (CompleteItemData (..), LspEnvironment)
import Language.PureScript.Names qualified as P
import Protolude

getMatchingImport :: (MonadIO m, MonadReader LspEnvironment m, MonadThrow m) => FilePath -> P.ModuleName -> m (Maybe Import)
getMatchingImport path moduleName' = do
  parseRes <- parseImportsFromFile path
  case parseRes of
    Left err -> do
      errorLsp $ "In " <> T.pack path <> " failed to parse imports from file: " <> err
      pure Nothing
    Right (_mn, _before, imports, _after) -> do
      pure $ find (\(Import _ _ mn) -> Just moduleName' == mn) imports

addImportToTextEdit :: (MonadIO m, MonadReader LspEnvironment m, MonadThrow m) => CompletionItem -> CompleteItemData -> m CompletionItem
addImportToTextEdit completionItem completeItemData = do
  importEdits <- getImportEdits completeItemData
  pure $ set LSP.additionalTextEdits importEdits completionItem

getImportEdits :: (MonadIO m, MonadReader LspEnvironment m, MonadThrow m) => CompleteItemData -> m (Maybe [TextEdit])
getImportEdits (CompleteItemData path moduleName' importedModuleName name word (Range wordStart _)) = do
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
          case addDeclarationToImports moduleName' importedModuleName wordQualifierMb decl imports of
            Nothing -> pure Nothing
            Just (newImports, moduleQualifier) -> do
              let importEdits = importsToTextEdit before newImports
                  qualifierEdits = case moduleQualifier of
                    Just qual | isNothing wordQualifierMb -> [TextEdit (Range wordStart wordStart) (P.runModuleName qual <> ".")]
                    _ -> []

              pure $ Just $ [importEdits] <> qualifierEdits
  where
    wordQualifierMb = fst <$> getIdentModuleQualifier word

getIdentModuleQualifier :: Text -> Maybe (P.ModuleName, Text)
getIdentModuleQualifier word =
  case T.splitOn "." word of
    [] -> Nothing
    [_] -> Nothing
    xs -> Just (P.ModuleName $ T.intercalate "." $ init xs, last xs)

addDeclarationToImports :: P.ModuleName -> P.ModuleName -> Maybe P.ModuleName -> P.Declaration -> [Import] -> Maybe ([Import], Maybe P.ModuleName)
addDeclarationToImports moduleName' importedModuleName wordQualifierMb decl imports
  | importingSelf = Nothing
  | Just existing <- alreadyImportedModuleMb,
    Just ref <- refMb = case existing of
      Import _ (P.Explicit refs') mName
        | wordQualifierMb == mName ->
            if ref `notElem` refs'
              then Just (Import importedModuleName (P.Explicit (refs' <> [ref])) Nothing : withoutOldImport, mName)
              else Nothing
        | otherwise -> Just (imports, mName)
      Import _ P.Implicit mName -> Just (imports, mName)
      Import _ (P.Hiding refs') mName
        | wordQualifierMb == mName ->
            if ref `elem` refs'
              then Just (Import importedModuleName (P.Hiding (filter (/= ref) refs')) Nothing : withoutOldImport, mName)
              else Nothing
        | otherwise -> Just (imports, mName)
  | isJust wordQualifierMb = Just (Import importedModuleName P.Implicit wordQualifierMb : imports, wordQualifierMb)
  | otherwise = addExplicitNewImport
  where
    addExplicitNewImport = Just (Import importedModuleName (P.Explicit refs) wordQualifierMb : imports, wordQualifierMb)
    withoutOldImport :: [Import]
    withoutOldImport = maybe identity (\im -> filter (/= im)) alreadyImportedModuleMb imports

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
        (LSP.Position (beforeLine + fromIntegral (length printed) + 1) 0)
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
