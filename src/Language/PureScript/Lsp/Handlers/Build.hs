{-# LANGUAGE TypeApplications #-}

module Language.PureScript.Lsp.Handlers.Build where

import Data.Aeson qualified as A
import Language.LSP.Protocol.Message qualified as Message
import Language.LSP.Protocol.Types qualified as Types
import Language.LSP.Server qualified as Server
import Language.PureScript qualified as P
import Language.PureScript.Compile (compile)
import Language.PureScript.Lsp.Cache (updateAvailableSrcs)
import Language.PureScript.Lsp.Diagnostics (errorMessageDiagnostic)
import Language.PureScript.Lsp.Monad (HandlerM)
import Language.PureScript.Lsp.Rebuild (codegenTargets)
import Language.PureScript.Make.Index (initDb)
import Protolude hiding (to)
import System.IO.UTF8 (readUTF8FilesT)
import Language.PureScript.Lsp.State (clearCache, getDbConn)
import Language.LSP.Server (getConfig, withIndefiniteProgress, ProgressCancellable (Cancellable))
import Language.PureScript.Lsp.ServerConfig (ServerConfig(outputPath))
import Language.LSP.Protocol.Lens qualified as LSP
import Control.Lens ((^.))

buildHandler :: Server.Handlers HandlerM
buildHandler =
  Server.requestHandler (Message.SMethod_CustomMethod $ Proxy @"build") $ \req res -> do
    let progressToken = cast $ req ^. LSP.id
    diags <- buildForLsp progressToken
    res $ Right $ A.toJSON diags

    where 

-- Either get progress to work or remove it
buildForLsp :: Maybe Types.ProgressToken -> HandlerM [Types.Diagnostic]
buildForLsp  id = do
  withIndefiniteProgress "Rebuilding all files" id Cancellable $ \updateProgress -> do 
    clearCache
    outDir <-  outputPath <$> getConfig
    conn <- getDbConn
    liftIO $ initDb conn
    updateProgress "Updating available sources"
    input <- updateAvailableSrcs
    updateProgress "Reading module files"
    moduleFiles <- liftIO $ readUTF8FilesT input
    updateProgress "Compiling"
    (result, warnings) <-
      liftIO $
        compile
          (P.Options False False codegenTargets)
          moduleFiles
          conn
          outDir
          False
    pure $
      (errorMessageDiagnostic Types.DiagnosticSeverity_Error <$> either P.runMultipleErrors (const []) result)
        <> (errorMessageDiagnostic Types.DiagnosticSeverity_Warning <$> P.runMultipleErrors warnings)