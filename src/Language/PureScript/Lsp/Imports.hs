module Language.PureScript.Lsp.Imports
  ( getMatchingImport,
    addImportToTextEdit,
    getIdentModuleQualifier,
    parseModuleNameFromFile,
  )
where

import Control.Lens (set)
import Control.Monad.Catch (MonadThrow)
import Data.List (nub)
import Data.Maybe as Maybe
import Data.Text qualified as T
import Data.Text.Utf16.Rope.Mixed qualified as Rope
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Types as LSP
import Language.LSP.Server (MonadLsp)
import Language.PureScript (DeclarationRef)
import Language.PureScript.AST.Declarations qualified as P
import Language.PureScript.AST.SourcePos (nullSourceSpan)
import Language.PureScript.CST qualified as CST
import Language.PureScript.CST.Monad qualified as CSTM
import Language.PureScript.Ide.Imports (Import (Import), prettyPrintImportSection, sliceImportSection)
import Language.PureScript.Lsp.Cache.Query (getAstDeclarationInModule)
import Language.PureScript.Lsp.Log (debugLsp, errorLsp, warnLsp)
import Language.PureScript.Lsp.NameType (LspNameType (..))
import Language.PureScript.Lsp.ReadFile (lspReadFileRope)
import Language.PureScript.Lsp.ServerConfig (ServerConfig)
import Language.PureScript.Lsp.Types (CompleteItemData (..), LspEnvironment)
import Language.PureScript.Lsp.Util (filePathToNormalizedUri)
import Language.PureScript.Names qualified as P
import Protolude

getMatchingImport :: (MonadLsp ServerConfig m, MonadReader LspEnvironment m, MonadThrow m) => NormalizedUri -> P.ModuleName -> m (Maybe Import)
getMatchingImport path moduleName' = do
  parseRes <- parseImportsFromFile path
  case parseRes of
    Left err -> do
      errorLsp $ "In " <> show path <> " failed to parse imports from file: " <> err
      pure Nothing
    Right (_mn, _before, imports, _after) -> do
      pure $ find (\(Import _ _ mn) -> Just moduleName' == mn) imports

addImportToTextEdit :: (MonadLsp ServerConfig m, MonadReader LspEnvironment m, MonadThrow m) => CompletionItem -> CompleteItemData -> m CompletionItem
addImportToTextEdit completionItem completeItemData = do
  importEdits <- getImportEdits completeItemData
  pure $ set LSP.additionalTextEdits importEdits completionItem

getImportEdits :: (MonadLsp ServerConfig m, MonadReader LspEnvironment m, MonadThrow m) => CompleteItemData -> m (Maybe [TextEdit])
getImportEdits cid@(CompleteItemData path moduleName' importedModuleName name nameType word (Range wordStart _)) = do
  debugLsp $ "CompletionItemData: " <> show cid
  debugLsp $ "wordQualifierMb: " <> show (getIdentModuleQualifier word)
  parseRes <- parseImportsFromFile (filePathToNormalizedUri path)
  case parseRes of
    Left err -> do
      errorLsp $ "In " <> T.pack path <> " failed to parse imports from file: " <> err
      pure Nothing
    Right (_mn, before, imports, _after) -> do
      declMb <- getAstDeclarationInModule importedModuleName name nameType
      case declMb of
        Nothing -> do
          errorLsp $ "In " <> T.pack path <> " failed to get declaration from module: " <> show (importedModuleName, name, nameType)
          pure Nothing
        Just (declName, declType) -> do
          debugLsp $ "Got declaration: " <> show (declName, declType)
          case addDeclarationToImports moduleName' importedModuleName wordQualifierMb declName declType nameType imports of
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
  case parseRest (parseOne CST.parseExprP) word of
    Just (CST.ExprIdent _ (CST.QualifiedName _ (Just modName) ident)) ->
      Just (modName, CST.getIdent ident)
    _ -> Nothing

parseOne :: CST.Parser a -> CST.Parser a
parseOne p = CSTM.token CST.TokLayoutStart *> p <* CSTM.token CST.TokLayoutEnd

parseRest :: CST.Parser a -> Text -> Maybe a
parseRest p =
  fmap snd
    . hush
    . CST.runTokenParser (p <* CSTM.token CST.TokEof)
    . CST.lexTopLevel

addDeclarationToImports ::
  P.ModuleName ->
  P.ModuleName ->
  Maybe P.ModuleName ->
  Text ->
  Text ->
  Maybe LspNameType ->
  [Import] ->
  Maybe
    ( [Import], -- new imports 
      Maybe P.ModuleName -- module qualifier
    )
addDeclarationToImports moduleName' importedModuleName wordQualifierMb declName declType nameType imports
  | importingSelf = Nothing
  | Just existing <- alreadyImportedModuleMb,
    Just ref <- refMb = case existing of
      Import _ (P.Explicit refs') mName
        | wordQualifierMb == mName -> Just (Import importedModuleName (P.Explicit (insertImportRef ref refs')) Nothing : withoutOldImport, mName)
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
      nameType >>= \case
        IdentNameType -> Just $ P.ValueRef nullSourceSpan (P.Ident declName)
        ValOpNameType -> Just $ P.ValueOpRef nullSourceSpan (P.OpName declName)
        TyNameType -> Just $ P.TypeRef nullSourceSpan (P.ProperName declName) Nothing
        TyOpNameType -> Just $ P.TypeOpRef nullSourceSpan (P.OpName declName)
        DctorNameType -> Just $ P.TypeRef nullSourceSpan (P.ProperName declType) (Just [P.ProperName declName])
        TyClassNameType -> Just $ P.TypeClassRef nullSourceSpan (P.ProperName declName)
        ModNameType -> Just $ P.ModuleRef nullSourceSpan (P.ModuleName declName)

    alreadyImportedModuleMb =
      find (\(Import mn' _ _) -> mn' == importedModuleName) imports

    importingSelf = moduleName' == importedModuleName

insertImportRef :: DeclarationRef -> [DeclarationRef] -> [DeclarationRef]
insertImportRef (P.TypeRef _ ty ctrs) ((P.TypeRef ss ty' ctrs') : refs)
  | ty == ty' = P.TypeRef ss ty (nub <$> liftA2 (<>) ctrs ctrs') : refs
insertImportRef ref (ref' : refs)
  | ref == ref' = ref' : refs
  | otherwise = ref' : insertImportRef ref refs
insertImportRef ref [] = [ref]

importsToTextEdit :: [Text] -> [Import] -> TextEdit
importsToTextEdit before imports =
  TextEdit
    ( LSP.Range
        (LSP.Position beforeLine 0)
        ( LSP.Position
            ( beforeLine + fromIntegral (length printed) - 1
            )
            (maybe 0 (fromIntegral . T.length) $ lastMay printed)
        )
    )
    (T.unlines printed)
  where
    beforeLine = fromIntegral $ length before
    printed = prettyPrintImportSection imports

-- | Reads a file and returns the (lines before the imports, the imports, the
-- lines after the imports)
parseImportsFromFile ::
  (MonadThrow m, MonadLsp ServerConfig m) =>
  NormalizedUri ->
  m (Either Text (P.ModuleName, [Text], [Import], [Text]))
parseImportsFromFile fp = do
  rope <- lspReadFileRope fp
  pure $ sliceImportSection (Rope.lines rope)

parseModuleNameFromFile ::
  (MonadThrow m, MonadLsp ServerConfig m, MonadReader LspEnvironment m) =>
  NormalizedUri ->
  m (Maybe P.ModuleName)
parseModuleNameFromFile =
  parseImportsFromFile >=> \case
    Left err -> do
      warnLsp $ "Failed to parse module name from file: " <> err
      pure Nothing
    Right (mn, _, _, _) -> pure $ Just mn
