module Language.PureScript.LspSimple (main) where

import Control.Monad.IO.Unlift
import Data.IORef (newIORef)
import Data.Map qualified as Map
import Language.LSP.Protocol.Types qualified as Types
import Language.LSP.Server qualified as Server
import Language.PureScript.Lsp.Types (LspEnvironment)
import Protolude hiding (to)
import Language.PureScript.Lsp.Handlers (handlers)

main :: LspEnvironment -> IO Int
main lspEnv = do
  diagErrs <- newIORef Map.empty
  Server.runServer $
    Server.ServerDefinition
      { parseConfig = const $ const $ Right (),
        onConfigChange = const $ pure (),
        defaultConfig = (),
        configSection = "oa-purescript-lsp",
        doInitialize = \env _req -> pure $ Right env,
        staticHandlers = \_caps -> handlers diagErrs,
        interpretHandler = \serverEnv ->
          Server.Iso
            ( Server.runLspT serverEnv . flip runReaderT lspEnv
            )
            liftIO,
        options = lspOptions
      }

syncOptions :: Types.TextDocumentSyncOptions
syncOptions =
  Types.TextDocumentSyncOptions
    { Types._openClose = Just True,
      Types._change = Just Types.TextDocumentSyncKind_Incremental,
      Types._willSave = Just False,
      Types._willSaveWaitUntil = Just False,
      Types._save = Just $ Types.InR $ Types.SaveOptions $ Just False
    }

lspOptions :: Server.Options
lspOptions =
  Server.defaultOptions
    { Server.optTextDocumentSync = Just syncOptions,
      Server.optExecuteCommandCommands = Just ["lsp-purescript-command"]
    }
