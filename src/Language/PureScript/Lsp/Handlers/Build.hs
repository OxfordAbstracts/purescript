{-# LANGUAGE TypeApplications #-}

module Language.PureScript.Lsp.Handlers.Build where

import Data.Aeson qualified as A
import Language.LSP.Protocol.Message qualified as Message
import Language.LSP.Protocol.Types qualified as Types
import Language.LSP.Server (getConfig)
import Language.LSP.Server qualified as Server
import Language.PureScript qualified as P
import Language.PureScript.Compile (compile)
import Language.PureScript.Lsp.Cache (updateAvailableSrcs)
import Language.PureScript.Lsp.Diagnostics (errorMessageDiagnostic, addJsonEdits)
import Language.PureScript.Lsp.Monad (HandlerM)
import Language.PureScript.Lsp.Rebuild (codegenTargets)
import Language.PureScript.Lsp.ServerConfig (ServerConfig (outputPath))
import Language.PureScript.Lsp.State (clearCache, getDbConn)
import Language.PureScript.Make.Index (initDb)
import Protolude hiding (to)
import System.IO.UTF8 (readUTF8FilesT)
import Language.PureScript.Lsp.Log (debugLsp)

buildHandler :: Server.Handlers HandlerM
buildHandler =
  Server.requestHandler (Message.SMethod_CustomMethod $ Proxy @"build") $ \_req res -> do
    diags <- buildForLsp
    res $ Right $ A.toJSON diags

-- Either get progress to work or remove it
buildForLsp :: HandlerM [Types.Diagnostic]
buildForLsp = do
  clearCache
  outDir <- outputPath <$> getConfig
  conn <- getDbConn
  liftIO $ initDb conn
  debugLsp "Updating available sources"
  input <- updateAvailableSrcs
  debugLsp "Reading module files"
  moduleFiles <- liftIO $ readUTF8FilesT input
  debugLsp "Compiling"
  (result, warnings) <-
    liftIO $
      compile
        (P.Options False False codegenTargets)
        moduleFiles
        conn
        outDir
        False
  config <- getConfig
  pure $ addJsonEdits $
    (errorMessageDiagnostic config Types.DiagnosticSeverity_Error <$> either P.runMultipleErrors (const []) result)
      <> (errorMessageDiagnostic config Types.DiagnosticSeverity_Warning <$> P.runMultipleErrors warnings)