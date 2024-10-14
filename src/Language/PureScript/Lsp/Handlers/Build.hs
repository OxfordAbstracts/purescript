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
import Language.PureScript.Lsp.Types (LspConfig (confOutputPath), LspEnvironment (lspConfig, lspDbConnection))
import Language.PureScript.Make.Index (initDb)
import Protolude hiding (to)
import System.IO.UTF8 (readUTF8FilesT)
import Language.PureScript.Lsp.State (clearCache)

buildHandler :: Server.Handlers HandlerM
buildHandler =
  Server.requestHandler (Message.SMethod_CustomMethod $ Proxy @"build") $ \_req res -> do
    clearCache
    config <- asks lspConfig
    conn <- asks lspDbConnection
    liftIO $ initDb conn
    input <- updateAvailableSrcs
    moduleFiles <- liftIO $ readUTF8FilesT input
    (result, warnings) <-
      liftIO $
        compile
          (P.Options False False codegenTargets)
          moduleFiles
          conn
          (confOutputPath config)
          False
    let diags :: [Types.Diagnostic]
        diags =
          (errorMessageDiagnostic Types.DiagnosticSeverity_Error <$> either P.runMultipleErrors (const []) result)
            <> (errorMessageDiagnostic Types.DiagnosticSeverity_Warning <$> P.runMultipleErrors warnings)
    res $ Right $ A.toJSON diags