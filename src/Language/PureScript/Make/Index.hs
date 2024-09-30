{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Language.PureScript.Make.Index where

import Control.Monad.Cont (MonadIO)
import Control.Monad.Supply (SupplyT (SupplyT))
import Data.Aeson qualified as A
import Data.List qualified as List
import Data.Map.Lazy qualified as M
import Data.Maybe (fromJust)
import Data.Set qualified as S
import Data.Set qualified as Set
import Data.Text qualified as T
import Database.SQLite.Simple (Connection)
import Database.SQLite.Simple qualified as SQL
import Language.PureScript.AST qualified as P
import Language.PureScript.CST qualified as CST
import Language.PureScript.CoreFn qualified as CF
import Language.PureScript.CoreFn.FromJSON qualified as CFJ
import Language.PureScript.CoreFn.ToJSON qualified as CFJ
import Language.PureScript.CoreFn.Traversals (traverseCoreFn)
import Language.PureScript.Errors qualified as P
import Language.PureScript.Externs (ExternsFile (efModuleName))
import Language.PureScript.Externs qualified as P
import Language.PureScript.Ide.Error (IdeError (GeneralError, RebuildError))
import Language.PureScript.Ide.Rebuild (updateCacheDb)
import Language.PureScript.Ide.Types (ModuleMap)
import Language.PureScript.Ide.Util (ideReadFile)
import Language.PureScript.Lsp.Cache
import Language.PureScript.Lsp.State (cacheRebuild)
import Language.PureScript.Lsp.Types (LspConfig (..), LspEnvironment (lspConfig))
import Language.PureScript.Make (ffiCodegen')
import Language.PureScript.Make qualified as P
import Language.PureScript.ModuleDependencies qualified as P
import Language.PureScript.Names qualified as P
import Language.PureScript.Options qualified as P
import Paths_purescript qualified as Paths
import Protolude hiding (moduleName)
import "monad-logger" Control.Monad.Logger (MonadLogger, logDebugN)

initDb :: Connection -> IO ()
initDb conn = do
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS corefn_modules (name TEXT PRIMARY KEY, path TEXT)"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS corefn_imports (module TEXT, imported_module TEXT)"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS corefn_declarations (module_name TEXT, ident TEXT, top_level BOOLEAN, value TEXT, start_line INTEGER, end_line INTEGER, start_col INTEGER, end_col INTEGER)"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS corefn_expressions (module_name TEXT, value TEXT, start_line INTEGER, end_line INTEGER, start_col INTEGER, end_col INTEGER, lines INTEGER, cols INTEGER)"

addCoreFnIndexing :: (MonadIO m) => Connection -> P.MakeActions m -> P.MakeActions m
addCoreFnIndexing getConn ma =
  ma
    { P.codegen = \m docs ext -> lift (indexCoreFn getConn m) <* P.codegen ma m docs ext
    }

indexCoreFn :: forall m. (MonadIO m) => Connection -> CF.Module CF.Ann -> m ()
indexCoreFn conn m = do
  liftIO do
    let mName = P.runModuleName $ CF.moduleName m
    SQL.execute
      conn
      (SQL.Query "INSERT INTO corefn_modules (name, path) VALUES (?, ?)")
      ( mName,
        T.pack $ CF.modulePath m
      )

    forM_ (CF.moduleImports m) \((span, _, _), importedModule) -> do
      SQL.execute
        conn
        (SQL.Query "INSERT INTO corefn_imports (module, imported_module) VALUES (?, ?)")
        ( mName,
          P.runModuleName importedModule
        )

    forM_ (CF.moduleDecls m) \bind -> do
      void $ insertBind mName True bind
      let (insertBind', _, _, _) = traverseCoreFn (insertBind mName False) (insertExpr mName) pure pure
      void $ insertBind' bind
  where
    insertBind :: Text -> Bool -> CF.Bind CF.Ann -> IO (CF.Bind CF.Ann)
    insertBind mName topLevel bind = do
      case bind of
        CF.NonRec (ss, _comments, _meta) ident expr -> do
          let
          SQL.execute
            conn
            ( SQL.Query
                "INSERT INTO corefn_declarations (module_name, ident, top_level, value, start_line, end_line, start_col, end_col) \
                \VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
            )
            ( mName,
              P.runIdent ident,
              topLevel,
              A.encode $ CFJ.bindToJSON bind,
              P.sourcePosLine $ P.spanStart ss,
              P.sourcePosLine $ P.spanEnd ss,
              P.sourcePosColumn $ P.spanStart ss,
              P.sourcePosColumn $ P.spanEnd ss
            )
        CF.Rec binds -> forM_ binds $ \((ann, ident), expr) ->
          void $ insertBind mName topLevel (CF.NonRec ann ident expr)
      pure bind

    insertExpr :: Text -> CF.Expr CF.Ann -> IO (CF.Expr CF.Ann)
    insertExpr mName expr = do
      SQL.execute
        conn
        ( SQL.Query
            "INSERT INTO corefn_expressions (module_name, value, start_line, end_line, start_col, end_col, lines, cols)\
            \ VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
        )
        ( mName,
          A.encode $ CFJ.exprToJSON expr,
          P.sourcePosLine start,
          P.sourcePosLine end,
          P.sourcePosColumn start,
          P.sourcePosColumn end,
          lines',
          cols
        )
      pure expr
      where
        (ss, _comments, _meta) = CF.extractAnn expr
        start = P.spanStart ss
        end = P.spanEnd ss
        lines' = P.sourcePosLine end - P.sourcePosLine start
        cols = P.sourcePosColumn end - P.sourcePosColumn start
