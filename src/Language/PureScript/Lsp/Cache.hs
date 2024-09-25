{-# LANGUAGE PackageImports #-}

module Language.PureScript.Lsp.Cache where

import Codec.Serialise (deserialise, serialise)
import Control.Lens (Field1 (_1), (^.), _1, _2, _3)
import Control.Monad.RWS (asks)
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as LB
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Database.SQLite.Simple
import Language.PureScript (prettyPrintBinder)
import Language.PureScript qualified as P
import Language.PureScript.AST.Declarations (declSourceAnn)
import Language.PureScript.AST.Exported (isExported)
import Language.PureScript.AST.Traversals (accumTypes)
import Language.PureScript.CST qualified as CST
import Language.PureScript.Externs (ExternsFile (efModuleName), externsFileName)
import Language.PureScript.Ide.Error (IdeError (GeneralError, RebuildError))
import Language.PureScript.Ide.Externs (convertExterns, readExternFile)
import Language.PureScript.Ide.State (convertDeclaration', resolveDataConstructorsForModule, resolveDocumentationForModule)
import Language.PureScript.Ide.Types (Annotation (_annDocumentation, _annExportedFrom, _annLocation, _annTypeAnnotation), IdeDeclaration (..), IdeDeclarationAnn (IdeDeclarationAnn, _idaAnnotation, _idaDeclaration), IdeNamespace (IdeNSModule, IdeNSType, IdeNSValue), ideDtorType, ideSynonymKind, ideTCKind, ideTypeKind, ideTypeOpKind, ideValueOpType, ideValueType, ModuleMap)
import Language.PureScript.Ide.Util (ideReadFile)
import Language.PureScript.Lsp.DB qualified as DB
import Language.PureScript.Lsp.Types (LspConfig (..), LspEnvironment (lspConfig))
import Language.PureScript.Pretty.Types (prettyPrintType)
import Language.PureScript.Sugar.BindingGroups (usedTypeNames)
import Protolude
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, getDirectoryContents)
import System.FilePath (normalise, (</>))
import System.FilePath.Glob (glob)
import "monad-logger" Control.Monad.Logger (MonadLogger)

-- loadCache ::
--   ( MonadIO m,
--     MonadLogger m,
--     MonadError IdeError m,
--     MonadReader LspEnvironment m
--   ) =>
--   m [(FilePath, [CST.ParserWarning])]
-- loadCache = do
--   globs <- asks (confGlobs . lspConfig)
--   files <- liftIO $ concat <$> traverse glob globs
--   traverse rebuildFile files

selectAllExternsMap :: (MonadIO m, MonadReader LspEnvironment m) => m (ModuleMap ExternsFile)
selectAllExternsMap = do
  Map.fromList . fmap (\ef -> (efModuleName ef, ef)) <$> selectAllExterns

selectAllExterns :: (MonadIO m, MonadReader LspEnvironment m) => m [ExternsFile]
selectAllExterns = do
  DB.query_ (Query "SELECT value FROM externs") <&> fmap (deserialise . fromOnly)

insertAllExterns ::
  ( MonadIO m,
    MonadReader LspEnvironment m,
    MonadError IdeError m,
    MonadLogger m
  ) =>
  m ()
insertAllExterns = do
  oDir <- asks (confOutputPath . lspConfig)
  externPaths <- findAvailableExterns
  forM_ externPaths $ \name -> do
    extern <- readExternFile (oDir </> T.unpack (P.runModuleName name) <> externsFileName)
    insertExtern oDir extern

-- | Finds all the externs inside the output folder and returns the
-- corresponding module names
findAvailableExterns :: (MonadIO m, MonadReader LspEnvironment m, MonadError IdeError m) => m [P.ModuleName]
findAvailableExterns = do
  oDir <- asks (confOutputPath . lspConfig)
  unlessM
    (liftIO (doesDirectoryExist oDir))
    (throwError (GeneralError $ "Couldn't locate your output directory at: " <> T.pack (normalise oDir)))
  liftIO $ do
    directories <- getDirectoryContents oDir
    moduleNames <- filterM (containsExterns oDir) directories
    pure (P.moduleNameFromString . toS <$> moduleNames)
  where
    -- Takes the output directory and a filepath like "Data.Array" and
    -- looks up, whether that folder contains an externs file
    containsExterns :: FilePath -> FilePath -> IO Bool
    containsExterns oDir d
      | d `elem` [".", ".."] = pure False
      | otherwise = do
          let file = oDir </> d </> P.externsFileName
          doesFileExist file

insertExtern ::
  (MonadIO m, MonadReader LspEnvironment m) =>
  FilePath ->
  ExternsFile ->
  m ()
insertExtern outDir extern = do
  DB.executeNamed
    (Query "INSERT INTO externs (name, path) VALUES (:path, :name)")
    [ ":path" := externsPath,
      ":ef_version" := P.efVersion extern,
      ":value" := serialise extern,
      ":module_name" := P.runModuleName name
    ]
  forM_ (P.efImports extern) $ insertEfImport name
  forM_ (P.efExports extern) $ insertEfExport name
  where
    externsPath = outDir </> T.unpack (P.runModuleName name) <> externsFileName
    name = efModuleName extern

insertEfImport :: (MonadIO m, MonadReader LspEnvironment m) => P.ModuleName -> P.ExternsImport -> m ()
insertEfImport moduleName' ei = do
  DB.executeNamed
    (Query "INSERT INTO ef_imports (module_name, import_name) VALUES (:module_name, :import_name)")
    [ ":module_name" := P.runModuleName moduleName',
      ":imported_module" := P.runModuleName (P.eiModule ei),
      ":import_type" := serialise (P.eiImportType ei),
      ":imported_as" := fmap P.runModuleName (P.eiImportedAs ei)
    ]

insertEfExport :: (MonadIO m, MonadReader LspEnvironment m) => P.ModuleName -> P.DeclarationRef -> m ()
insertEfExport moduleName' dr = do
  DB.executeNamed
    (Query "INSERT INTO ef_exports (module_name, export_name) VALUES (:module_name, :export_name)")
    [ ":module_name" := P.runModuleName moduleName',
      ":value" := serialise dr,
      ":span_name" := P.spanName span,
      ":start_col" := (P.sourcePosColumn . P.spanStart) span,
      ":start_line" := (P.sourcePosLine . P.spanStart) span,
      ":end_col" := (P.sourcePosColumn . P.spanEnd) span,
      ":end_line" := (P.sourcePosLine . P.spanEnd) span
    ]
  where
    span = P.declRefSourceSpan dr

insertModule :: (MonadIO m, MonadReader LspEnvironment m) => FilePath -> P.Module -> m ()
insertModule srcPath m = do
  let moduleName' = P.getModuleName m
  DB.executeNamed
    (Query "INSERT INTO modules (module_name, module) VALUES (:module_name, :module)")
    [ ":module_name" := P.runModuleName moduleName',
      ":path" := srcPath
    ]

  let exported = Set.fromList $ P.exportedDeclarations m
  traverse_ (insertDeclaration moduleName' exported) (P.getModuleDeclarations m)

insertDeclaration :: (MonadIO m, MonadReader LspEnvironment m) => P.ModuleName -> Set P.Declaration -> P.Declaration -> m ()
insertDeclaration moduleName' exportedDecls decl = do
  DB.executeNamed
    (Query "INSERT INTO declarations (module_name, declaration) VALUES (:module_name, :name, :value)")
    [ ":module_name" := P.runModuleName moduleName',
      ":name" := P.spanName declLocation,
      ":type_printed" := typeName,
      ":start_col" := (P.sourcePosColumn . P.spanStart) declLocation,
      ":start_line" := (P.sourcePosLine . P.spanStart) declLocation,
      ":end_col" := (P.sourcePosColumn . P.spanEnd) declLocation,
      ":end_line" := (P.sourcePosLine . P.spanEnd) declLocation,
      ":comments" := encode comments,
      ":exported" := exported,
      ":value" := serialise decl
    ]
  where
    typeName = Protolude.fold $ head typeNames

    typeNames :: [Text]
    typeNames = accumTypes (pure . T.pack . prettyPrintType maxBound) ^. _1 $ decl

    exported = Set.member decl exportedDecls
    (declLocation, comments) = declSourceAnn decl

resolveLocations :: (MonadIO m, MonadReader LspEnvironment m) => [IdeDeclarationAnn] -> m [IdeDeclarationAnn]
resolveLocations = traverse resolveLocation

resolveLocation :: (MonadIO m, MonadReader LspEnvironment m) => IdeDeclarationAnn -> m IdeDeclarationAnn
resolveLocation (IdeDeclarationAnn ann d) =
  convertDeclaration'
    annotateFunction
    annotateValue
    annotateDataConstructor
    annotateType
    annotateType -- type classes live in the type namespace
    annotateModule
    d
  where
    -- annotateFunction :: _
    annotateFunction x d' = do
      def <- selectIdentSourceSpan IdeNSValue $ P.runIdent x
      type' <- selectIdentSourceType x
      pure $
        IdeDeclarationAnn
          ( ann
              { _annLocation = def,
                _annTypeAnnotation = type'
              }
          )
          d'
    annotateValue x d' = do
      def <- selectIdentSourceSpan IdeNSValue x
      pure $ IdeDeclarationAnn (ann {_annLocation = def}) d'
    annotateDataConstructor x d' = do
      def <- selectIdentSourceSpan IdeNSValue x
      pure $ IdeDeclarationAnn (ann {_annLocation = def}) d'
    annotateType x d' = do
      def <- selectIdentSourceSpan IdeNSType x
      pure $ IdeDeclarationAnn (ann {_annLocation = def}) d'
    annotateModule x d' = do
      def <- selectIdentSourceSpan IdeNSModule x
      pure $ IdeDeclarationAnn (ann {_annLocation = def}) d'

insertIdentSourceSpan :: (MonadIO m, MonadReader LspEnvironment m) => IdeNamespace -> Text -> P.SourceSpan -> m ()
insertIdentSourceSpan nameSpace ident span =
  DB.executeNamed
    (Query "INSERT INTO ident_source_spans (ident, start_col, start_line, end_col, end_line) VALUES (:ident, :start_col, :start_line, :end_col, :end_line :name_space)")
    [ ":ident" := ident,
      ":start_col" := P.sourcePosColumn (P.spanStart span),
      ":start_line" := P.sourcePosLine (P.spanStart span),
      ":end_col" := P.sourcePosColumn (P.spanEnd span),
      ":end_line" := P.sourcePosLine (P.spanEnd span),
      ":name_space" := serialise nameSpace
    ]

selectIdentSourceSpan :: (MonadIO m, MonadReader LspEnvironment m) => IdeNamespace -> Text -> m (Maybe P.SourceSpan)
selectIdentSourceSpan name_space ident =
  fmap toSpan . head
    <$> DB.queryNamed
      (Query "SELECT start_col, start_line, end_col, end_line FROM ident_source_spans WHERE ident = :ident and name_space = :name_space")
      [ ":ident" := ident,
        ":name_space" := serialise name_space
      ]
  where
    toSpan :: (Int, Int, Int, Int) -> P.SourceSpan
    toSpan (startCol, startLine, endCol, endLine) =
      P.SourceSpan
        (T.unpack ident)
        (P.SourcePos startLine startCol)
        (P.SourcePos endLine endCol)

insertIdentSourceType :: (MonadIO m, MonadReader LspEnvironment m) => P.Ident -> P.SourceType -> m ()
insertIdentSourceType ident ty =
  DB.executeNamed
    (Query "INSERT INTO ident_source_types (ident, type) VALUES (:ident, :type)")
    [":ident" := P.runIdent ident, ":type" := serialise ty]

selectIdentSourceType :: (MonadIO m, MonadReader LspEnvironment m) => P.Ident -> m (Maybe P.SourceType)
selectIdentSourceType ident =
  fmap (deserialise . fromOnly) . head
    <$> DB.queryNamed
      (Query "SELECT type FROM ident_source_types WHERE ident = :ident")
      [":ident" := P.runIdent ident]