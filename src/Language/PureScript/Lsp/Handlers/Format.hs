module Language.PureScript.Lsp.Handlers.Format where

import Control.Lens ((^.))
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as Message
import Language.LSP.Protocol.Types qualified as Types
import Language.LSP.Server qualified as Server
import Language.PureScript.Lsp.Monad (HandlerM)
import Language.PureScript.Lsp.ReadFile (lspReadFileText)
import Protolude
import System.Process (readProcess)

formatHandler :: Server.Handlers HandlerM
formatHandler = Server.requestHandler Message.SMethod_TextDocumentFormatting $ \req res -> do
  let uri = req ^. LSP.params . LSP.textDocument . LSP.uri
  contents <- lspReadFileText $ Types.toNormalizedUri uri
  formatted <-  liftIO $ readProcess "purs-tidy" ["format"] (toS contents)
  res $ Right $ Types.InL [Types.TextEdit (Types.Range (Types.Position 0 0) (Types.Position 100000 0)) (toS formatted)]