module Language.PureScript.Lsp.Monad where

import Language.LSP.Server (LspT)
import Language.PureScript.Lsp.Types
import Protolude
import Language.PureScript.Lsp.ServerConfig (ServerConfig)

type HandlerM = ReaderT LspEnvironment (LspT ServerConfig IO) 
