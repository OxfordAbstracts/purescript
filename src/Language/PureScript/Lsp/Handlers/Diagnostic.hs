{-# LANGUAGE BlockArguments #-}

module Language.PureScript.Lsp.Handlers.Diagnostic where

import Control.Lens ((^.))
import Data.Aeson qualified as A
import Data.Map qualified as Map
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as Message
import Language.LSP.Protocol.Types qualified as Types
import Language.LSP.Server qualified as Server
import Language.PureScript.Lsp.Diagnostics (TitledTextEdit (..), getFileDiagnotics, getMsgUri)
import Language.PureScript.Lsp.Monad (HandlerM)
import Protolude hiding (to)

diagnosticAndCodeActionHandlers :: Server.Handlers HandlerM
diagnosticAndCodeActionHandlers =
  mconcat
    [ Server.requestHandler Message.SMethod_TextDocumentDiagnostic $ \req res -> do
        diagnostics <- getFileDiagnotics req
        res $
          Right $
            Types.DocumentDiagnosticReport $
              Types.InL $
                Types.RelatedFullDocumentDiagnosticReport Types.AString Nothing diagnostics Nothing,
      Server.requestHandler Message.SMethod_TextDocumentCodeAction $ \req res -> do
        let params = req ^. LSP.params
            diags :: [Types.Diagnostic]
            diags = params ^. LSP.context . LSP.diagnostics
            uri = getMsgUri req

        res $
          Right $
            Types.InL $
              diags >>= \diag ->
                let titledEdits :: [TitledTextEdit]
                    titledEdits = case A.fromJSON <$> diag ^. LSP.data_ of
                      Just (A.Success tes) -> tes
                      _ -> []

                    unusedImportEdits :: [Types.TextEdit]
                    unusedImportEdits = titledEdits >>= tteImportEdits

                    textEdits :: [Types.TextEdit]
                    textEdits = map tteEdit titledEdits

                    allEdits :: [Types.TextEdit]
                    allEdits = titledEdits >>= tteAllEdits
                 in [ Types.InR $
                        Types.CodeAction
                          (foldMap tteTitle $ head titledEdits)
                          (Just Types.CodeActionKind_QuickFix)
                          (Just [diag])
                          (Just True)
                          Nothing
                          ( Just $
                              Types.WorkspaceEdit
                                (Just $ Map.singleton uri textEdits)
                                Nothing
                                Nothing
                          )
                          Nothing
                          Nothing
                    ]
                      <> [ Types.InR $
                             Types.CodeAction
                               "Remove all unused imports"
                               (Just Types.CodeActionKind_QuickFix)
                               Nothing
                               (Just True)
                               Nothing
                               ( Just $
                                   Types.WorkspaceEdit
                                     (Just $ Map.singleton uri unusedImportEdits)
                                     Nothing
                                     Nothing
                               )
                               Nothing
                               Nothing
                           | length unusedImportEdits > 1
                         ]
                      <> [ Types.InR $
                             Types.CodeAction
                               "Apply all suggestions"
                               (Just Types.CodeActionKind_QuickFix)
                               (Just diags)
                               (Just True)
                               Nothing
                               ( Just $
                                   Types.WorkspaceEdit
                                     (Just $ Map.singleton uri allEdits)
                                     Nothing
                                     Nothing
                               )
                               Nothing
                               Nothing
                           | length allEdits > 1
                         ]
    ]
