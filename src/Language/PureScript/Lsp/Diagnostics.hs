module Language.PureScript.Lsp.Diagnostics where

import Control.Lens ((^.))
import Control.Monad.Catch (MonadThrow)
import Data.Aeson qualified as A
import Data.List.NonEmpty qualified as NEL
import Data.Text qualified as T
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Types (Diagnostic, Uri)
import Language.LSP.Protocol.Types qualified as Types
import Language.LSP.Server (MonadLsp)
import Language.PureScript qualified as P
import Language.PureScript.Errors (ErrorMessage (ErrorMessage), MultipleErrors (runMultipleErrors), errorCode, errorDocUri, errorSpan, noColorPPEOptions, prettyPrintSingleError)
import Language.PureScript.Errors qualified as Errors
import Language.PureScript.Errors.JSON (toSuggestion)
import Language.PureScript.Errors.JSON qualified as JsonErrors
import Language.PureScript.Lsp.Rebuild (RebuildResult (RebuildError, RebuildWarning), rebuildFile)
import Language.PureScript.Lsp.ServerConfig (ServerConfig)
import Language.PureScript.Lsp.Types (LspEnvironment)
import Protolude hiding (to)
import Text.PrettyPrint.Boxes (render)

getFileDiagnotics ::
  ( LSP.HasParams s a1,
    LSP.HasTextDocument a1 a2,
    LSP.HasUri a2 Uri,
    MonadLsp ServerConfig m,
    MonadThrow m,
    MonadReader LspEnvironment m
  ) =>
  s ->
  m [Diagnostic]
getFileDiagnotics msg = do
  let uri :: Types.NormalizedUri
      uri = getMsgUri msg & Types.toNormalizedUri
  res <- rebuildFile uri
  pure $ getResultDiagnostics res

getMsgUri :: (LSP.HasParams s a1, LSP.HasTextDocument a1 a2, LSP.HasUri a2 a3) => s -> a3
getMsgUri msg = msg ^. LSP.params . LSP.textDocument . LSP.uri

getResultDiagnostics ::
  RebuildResult ->
  [Types.Diagnostic]
getResultDiagnostics res = case res of
  RebuildError errors -> errorsToDiagnostics Types.DiagnosticSeverity_Error errors
  RebuildWarning errors -> errorsToDiagnostics Types.DiagnosticSeverity_Warning errors

errorsToDiagnostics :: Types.DiagnosticSeverity -> P.MultipleErrors -> [Types.Diagnostic]
errorsToDiagnostics severity errs =
  errorMessageDiagnostic severity <$> runMultipleErrors errs

errorMessageDiagnostic :: Types.DiagnosticSeverity -> ErrorMessage -> Types.Diagnostic
errorMessageDiagnostic severity msg@((ErrorMessage _hints _)) =
  let textEdits :: [Types.TextEdit]
      textEdits =
        toSuggestion msg
          & maybeToList
            >>= suggestionToEdit

      suggestionToEdit :: JsonErrors.ErrorSuggestion -> [Types.TextEdit]
      suggestionToEdit (JsonErrors.ErrorSuggestion replacement (Just JsonErrors.ErrorPosition {..})) =
        let rangeStart = Types.Position (fromIntegral $ startLine - 1) (fromIntegral $ startColumn - 1)
            rangeEnd = Types.Position (fromIntegral $ endLine - 1) (fromIntegral $ endColumn - 1)
         in pure $ Types.TextEdit (Types.Range rangeStart rangeEnd) replacement
      suggestionToEdit _ = []
   in Types.Diagnostic
        (Types.Range start end)
        (Just severity)
        (Just $ Types.InR $ errorCode msg)
        (Just $ Types.CodeDescription $ Types.Uri $ errorDocUri msg)
        (T.pack <$> spanName)
        (T.pack $ render $ prettyPrintSingleError noColorPPEOptions msg)
        Nothing
        Nothing
        (Just $ A.toJSON textEdits)
  where
    notFound = Types.Position 0 0
    (spanName, start, end) = getPositions $ errorSpan msg

    getPositions = fromMaybe (Nothing, notFound, notFound) . getPositionsMb

    getPositionsMb = fmap $ \spans ->
      let (Errors.SourceSpan name (Errors.SourcePos startLine startCol) (Errors.SourcePos endLine endCol)) =
            NEL.head spans
       in ( Just name,
            Types.Position (fromIntegral $ startLine - 1) (fromIntegral $ startCol - 1),
            Types.Position (fromIntegral $ endLine - 1) (fromIntegral $ endCol - 1)
          )
