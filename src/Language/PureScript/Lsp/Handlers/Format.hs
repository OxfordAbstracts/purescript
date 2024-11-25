module Language.PureScript.Lsp.Handlers.Format where

import Control.Lens ((^.))
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as Message
import Language.LSP.Protocol.Types qualified as Types
import Language.LSP.Server (getConfig)
import Language.LSP.Server qualified as Server
import Language.PureScript.Lsp.Imports (parseImportsFromFile, printImports)
import Language.PureScript.Lsp.Log (debugLsp, warnLsp)
import Language.PureScript.Lsp.Monad (HandlerM)
import Language.PureScript.Lsp.ReadFile (lspReadFileText)
import Language.PureScript.Lsp.ServerConfig (Formatter (..), ServerConfig (formatter))
import Protolude
import System.Process (readProcess)
import Data.String qualified as S

formatHandler :: Server.Handlers HandlerM
formatHandler = Server.requestHandler Message.SMethod_TextDocumentFormatting $ \req res -> do
  let uri = req ^. LSP.params . LSP.textDocument . LSP.uri
      normalizedUri = Types.toNormalizedUri uri
      filePath = Types.uriToFilePath uri
  debugLsp $ "Formatting file: " <> show filePath
  config <- getConfig
  case (formatter config, filePath) of
    (PursTidyFormatInPlace, Just fp) -> do
      void $ liftIO $ readProcess "purs-tidy" ["format-in-place", fp] []
      res $ Right $ Types.InR Types.Null
    (PursTidyFormatInPlace, Nothing) -> do
      res $ Left $ Message.TResponseError (Types.InR Types.ErrorCodes_InternalError) "File path not found" Nothing
    (PursTidy, _) -> do
      parsedImportsRes <- parseImportsFromFile normalizedUri
      contents <- case parsedImportsRes of
        Left err -> do
          warnLsp $ "Failed to parse imports from file: " <> err
          lspReadFileText normalizedUri
        Right imports -> pure $ printImports imports
      formatted <- liftIO $ readProcess "purs-tidy" ["format"] (toS contents)
      let lines' = toEnum $ max (length $ S.lines formatted) (length $ lines contents)
      res $ Right $ Types.InL [Types.TextEdit (Types.Range (Types.Position 0 0) (Types.Position (lines' + 1) 0)) (toS formatted)]
    _ -> res $ Left $ Message.TResponseError (Types.InR Types.ErrorCodes_InvalidParams) "No formatter set" Nothing
