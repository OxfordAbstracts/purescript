module Language.PureScript.Lsp.Cache where

import Codec.Serialise (serialise)
import Data.Aeson (encode)
import Database.SQLite.Simple
import Language.PureScript qualified as P
import Language.PureScript.AST (Module (..))
import Language.PureScript.Externs (ExternsFile)
import Language.PureScript.Lsp.DB (executeNamed')
import Language.PureScript.Lsp.Types (LspEnvironment)
import Protolude

loadModules :: (MonadIO m, MonadReader LspEnvironment m) => m ()
loadModules = pure ()

insertModule :: (MonadIO m, MonadReader LspEnvironment m) => FilePath -> ExternsFile -> Module -> m ()
insertModule fp extern (Module P.SourceSpan {..} comments name declarations exports) = do
  executeNamed'
    (Query "INSERT INTO modules (name, path) VALUES (:path, :name)")
    [ ":path" := fp,
      "ef_version" := P.efVersion extern,
      ":name" := P.runModuleName name,
      ":src" := spanName,
      ":start_col" := P.sourcePosColumn spanStart,
      ":start_line" := P.sourcePosLine spanStart,
      ":end_col" := P.sourcePosColumn spanEnd,
      ":end_line" := P.sourcePosLine spanEnd,
      "comments" := encode comments
    ]

  forM_ (P.efImports extern) $ insertEfImport name
  forM_ declarations $ insertDeclaration name

insertEfImport :: (MonadIO m, MonadReader LspEnvironment m) => P.ModuleName -> P.ExternsImport -> m ()
insertEfImport moduleName' ei = do
  executeNamed'
    (Query "INSERT INTO ef_imports (module_name, import_name) VALUES (:module_name, :import_name)")
    [ ":module_name" := P.runModuleName moduleName',
      ":imported_module_name" := P.runModuleName (P.eiModule ei),
      "import_type" := serialise (P.eiImportType ei),
      "imported_as" := fmap P.runModuleName (P.eiImportedAs ei)
    ]

insertDeclaration :: (MonadIO m, MonadReader LspEnvironment m) => P.ModuleName -> P.Declaration -> m ()
insertDeclaration moduleName' decl = do
  executeNamed'
    (Query "INSERT INTO declarations (module_name, declaration) VALUES (:module_name, :declaration)")
    [ ":module_name" := P.runModuleName moduleName',
    ]