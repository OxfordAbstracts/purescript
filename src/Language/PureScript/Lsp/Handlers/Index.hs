{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}

module Language.PureScript.Lsp.Handlers.Index (indexHandler) where

import Data.Aeson qualified as A
import Data.Text qualified as T
import Language.LSP.Protocol.Message qualified as Message
import Language.LSP.Server (MonadLsp, getConfig)
import Language.LSP.Server qualified as Server
import Language.PureScript (ExternsFile)
import Language.PureScript qualified as P
import Language.PureScript.Lsp.Log (errorLsp)
import Language.PureScript.Lsp.Monad (HandlerM)
import Language.PureScript.Lsp.ServerConfig (ServerConfig (outputPath))
import Language.PureScript.Lsp.Types (LspEnvironment)
import Language.PureScript.Make.Index (indexAstDeclFromExternDecl, indexExtern, initDb, indexAstModuleFromExtern)
import Language.PureScript.Make.Monad (readExternsFile)
import Protolude hiding (to)
import System.Directory (doesFileExist, getDirectoryContents)
import System.FilePath ((</>))
import Language.PureScript.Lsp.Handlers.DeleteOutput (deleteOutput)
import Language.PureScript.Lsp.Handlers.Build (buildForLsp)
import Language.PureScript.Lsp.State (getDbConn)

indexHandler :: Server.Handlers HandlerM
indexHandler =
  mconcat
    [ Server.requestHandler (Message.SMethod_CustomMethod $ Proxy @"index-fast") $ \_req res -> do
        conn <- getDbConn
        liftIO $ initDb conn
        externs <- findAvailableExterns
        for_ externs indexExternAndDecls
        res $ Right A.Null,
      Server.requestHandler (Message.SMethod_CustomMethod $ Proxy @"index-full") $ \_req res -> do
        conn <- getDbConn
        liftIO $ initDb conn
        deleteOutput
        diags <- buildForLsp
        res $ Right $ A.toJSON diags
    ]
  where
    indexExternAndDecls :: ExternsFile -> HandlerM ()
    indexExternAndDecls ef = do
      conn <- getDbConn
      indexExtern conn ef
      indexAstModuleFromExtern conn ef
      for_ (P.efDeclarations ef) (indexAstDeclFromExternDecl conn (P.efModuleName ef) (P.efDeclarations ef))

-- \| Finds all the externs inside the output folder and returns the
-- corresponding module names
findAvailableExterns ::
  forall m.
  ( MonadLsp ServerConfig m,
    MonadReader LspEnvironment m
  ) =>
  m [ExternsFile]
findAvailableExterns = do
  oDir <- outputPath <$> getConfig
  directories <- liftIO $ getDirectoryContents oDir
  moduleNames <- liftIO $ filterM (containsExterns oDir) directories
  catMaybes <$> for moduleNames (readExtern oDir)
  where
    -- Takes the output directory and a filepath like "Data.Array" and
    -- looks up, whether that folder contains an externs file
    containsExterns :: FilePath -> FilePath -> IO Bool
    containsExterns oDir d
      | d `elem` [".", ".."] = pure False
      | otherwise = do
          let file = oDir </> d </> P.externsFileName
          doesFileExist file

    readExtern :: FilePath -> FilePath -> m (Maybe ExternsFile)
    readExtern oDir fp = do
      let path = oDir </> fp </> P.externsFileName
      res <- runExceptT $ readExternsFile path
      case res of
        Left err -> do
          errorLsp $ "Error reading externs file: " <> T.pack (P.prettyPrintMultipleErrors P.noColorPPEOptions err)
          pure Nothing
        Right (Just ef) -> pure $ Just ef
        _ -> pure Nothing
