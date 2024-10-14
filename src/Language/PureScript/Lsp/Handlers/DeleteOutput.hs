{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}

module Language.PureScript.Lsp.Handlers.DeleteOutput where

import Data.Aeson qualified as A
import Language.LSP.Protocol.Message qualified as Message
import Language.LSP.Server qualified as Server
import Language.PureScript.DB (dbFile)
import Language.PureScript.Lsp.Monad (HandlerM)
import Language.PureScript.Lsp.Types (LspConfig (confOutputPath), LspEnvironment (lspConfig))
import Protolude hiding (to)
import System.Directory (createDirectoryIfMissing, listDirectory, removePathForcibly)
import System.FilePath ((</>))

deleteOutputHandler :: Server.Handlers HandlerM
deleteOutputHandler =
  Server.requestHandler (Message.SMethod_CustomMethod $ Proxy @"delete-output") $ \_req res -> do
    outDir <- asks (confOutputPath . lspConfig)
    liftIO $ createDirectoryIfMissing True outDir
    contents <- liftIO $ listDirectory outDir
    for_ contents \f -> do
      unless (f == dbFile || dbFile `isPrefixOf` f) do
        let path = outDir </> f
        liftIO $ removePathForcibly path
    res $ Right A.Null