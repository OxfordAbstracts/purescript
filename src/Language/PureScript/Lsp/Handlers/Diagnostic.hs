module Language.PureScript.Lsp.Handlers.Diagnostic where

import Control.Lens ((^.))
import Data.Aeson qualified as A
import Data.Map qualified as Map
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as Message
import Language.LSP.Protocol.Types qualified as Types
import Language.LSP.Server qualified as Server
import Language.PureScript.Lsp.Diagnostics (getFileDiagnotics, getMsgUri)
import Language.PureScript.Lsp.Monad (HandlerM)
import Protolude hiding (to)

diagnosticAndCodeActionHandlers :: Server.Handlers HandlerM
diagnosticAndCodeActionHandlers =
  mconcat
    [ Server.requestHandler Message.SMethod_TextDocumentDiagnostic $ \req res -> do
        (_errs, diagnostics) <- getFileDiagnotics req
        res $
          Right $
            Types.DocumentDiagnosticReport $
              Types.InL $
                Types.RelatedFullDocumentDiagnosticReport Types.AString Nothing diagnostics Nothing,
      Server.requestHandler Message.SMethod_TextDocumentCodeAction $ \req res -> do
        let params = req ^. LSP.params
            diags = params ^. LSP.context . LSP.diagnostics
            uri = getMsgUri req

        res $
          Right $
            Types.InL $
              diags <&> \diag ->
                let textEdits = case A.fromJSON <$> diag ^. LSP.data_ of
                      Just (A.Success tes) -> tes
                      _ -> []
                 in Types.InR $
                      Types.CodeAction
                        "Apply suggestion"
                        (Just Types.CodeActionKind_QuickFix)
                        (Just diags)
                        (Just True)
                        Nothing -- disabled
                        ( Just $
                            Types.WorkspaceEdit
                              (Just $ Map.singleton uri textEdits)
                              Nothing
                              Nothing
                        )
                        Nothing
                        Nothing
    ]