module Language.PureScript.Lsp.Monad where

import Language.LSP.Server (LspT)
import Language.PureScript.Lsp.Types
import Protolude

type HandlerM = HandlerMWithConfig ServerConfig

type HandlerMWithConfig config = ReaderT LspEnvironment (LspT config IO)