{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Language.PureScript.LspSimple where

import Data.Text qualified as T
import Language.LSP.Protocol.Message qualified as Message
import Language.LSP.Protocol.Types qualified as Types
import Language.LSP.Server qualified as Server
import Language.PureScript.Ide.Types (IdeEnvironment)
import Protolude

type HandlerM = Server.LspT () (ReaderT IdeEnvironment IO)

handlers :: Server.Handlers HandlerM
handlers =
  mconcat
    [ Server.notificationHandler Message.SMethod_Initialized $ \_not -> do
        let params =
              Types.ShowMessageRequestParams
                Types.MessageType_Info
                "Turn on code lenses?"
                (Just [Types.MessageActionItem "Turn on", Types.MessageActionItem "Don't"])
        _ <- Server.sendRequest Message.SMethod_WindowShowMessageRequest params $ \case
          Right (Types.InL (Types.MessageActionItem "Turn on")) -> do
            let regOpts = Types.CodeLensRegistrationOptions (Types.InR Types.Null) Nothing (Just False)

            _ <- Server.registerCapability mempty Message.SMethod_TextDocumentCodeLens regOpts $ \_req responder -> do
              let cmd = Types.Command "Say hello" "lsp-hello-command" Nothing
                  rsp = [Types.CodeLens (Types.mkRange 0 0 0 100) (Just cmd) Nothing]
              responder $ Right $ Types.InL rsp
            pure ()
          Right _ ->
            Server.sendNotification Message.SMethod_WindowShowMessage (Types.ShowMessageParams Types.MessageType_Info "Not turning on code lenses")
          Left err ->
            Server.sendNotification Message.SMethod_WindowShowMessage (Types.ShowMessageParams Types.MessageType_Error $ "Something went wrong!\n" <> T.pack (show err))
        pure (),
      Server.requestHandler Message.SMethod_TextDocumentHover $ \req responder -> do
        let Message.TRequestMessage _ _ _ (Types.HoverParams _doc pos _workDone) = req
            Types.Position _l _c' = pos
            rsp = Types.Hover (Types.InL ms) (Just range)
            ms = Types.mkMarkdown "Hello world"
            range = Types.Range pos pos
        responder (Right $ Types.InL rsp),
      Server.notificationHandler Message.SMethod_TextDocumentDidOpen $ \msg -> do
        pure ()
    ]

-- main :: IO Int
-- main =
--   Server.runServer $
--     Server.ServerDefinition
--       { parseConfig = const $ const $ Right (),
--         onConfigChange = const $ pure (),
--         defaultConfig = (),
--         configSection = "oa-purescript",
--         doInitialize = \env _req -> pure $ Right env,
--         staticHandlers = \_caps -> handlers,
--         interpretHandler = \env -> Server.Iso (Server.runLspT env) liftIO,
--         options = Server.defaultOptions
--       }