{-# LANGUAGE DeriveAnyClass #-}

module Language.PureScript.Lsp.Diagnostics (TitledTextEdit (..), addJsonEdits, errorMessageDiagnostic, getFileDiagnotics, getMsgUri) where

import Control.Lens (set, (^.))
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
import Language.PureScript.Lsp.Log (debugLsp)

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
  debugLsp $ "getting file diagnostics for: " <> show uri
  res <- rebuildFile uri
  pure $ addJsonEdits $ getResultDiagnostics res

addJsonEdits :: [(Types.Diagnostic, [TitledTextEdit])] -> [Types.Diagnostic]
addJsonEdits diags =
  let allEdits :: [Types.TextEdit]
      allEdits =
        if length diags > 1 then diags >>= fmap tteEdit . snd else []

      importEdits :: [Types.TextEdit]
      importEdits =
        if length diags > 1 then diags >>= fmap tteEdit . filter tteIsUnusedImport . snd else []
   in diags
        <&> \(diag, edits) ->
          let
            withApplyAlls = 
               edits
                 <&> addAllEdits allEdits
                  <&> addImportEdits importEdits

          in
           set LSP.data_ (Just $ A.toJSON withApplyAlls) diag

getMsgUri :: (LSP.HasParams s a1, LSP.HasTextDocument a1 a2, LSP.HasUri a2 a3) => s -> a3
getMsgUri msg = msg ^. LSP.params . LSP.textDocument . LSP.uri

getResultDiagnostics ::
  RebuildResult ->
  [(Types.Diagnostic, [TitledTextEdit])]
getResultDiagnostics res = case res of
  RebuildError errors -> errorsToDiagnostics Types.DiagnosticSeverity_Error errors
  RebuildWarning errors -> errorsToDiagnostics Types.DiagnosticSeverity_Warning errors

errorsToDiagnostics :: Types.DiagnosticSeverity -> P.MultipleErrors -> [(Types.Diagnostic, [TitledTextEdit])]
errorsToDiagnostics severity errs =
  errorMessageDiagnostic severity <$> runMultipleErrors errs

errorMessageDiagnostic :: Types.DiagnosticSeverity -> ErrorMessage -> (Types.Diagnostic, [TitledTextEdit])
errorMessageDiagnostic severity msg@((ErrorMessage _hints _)) =
  ( Types.Diagnostic
      (Types.Range start end)
      (Just severity)
      (Just $ Types.InR $ errorCode msg)
      (Just $ Types.CodeDescription $ Types.Uri $ errorDocUri msg)
      (T.pack <$> spanName)
      (T.pack $ render $ prettyPrintSingleError noColorPPEOptions $ Errors.withoutPosition $ Errors.withoutModule msg)
      Nothing
      Nothing
      Nothing,
    maybeToList (getErrorTextEdit msg)
  )
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

getErrorTextEdit :: ErrorMessage -> Maybe TitledTextEdit
getErrorTextEdit msg = do
  edit <- toSuggestion msg >>= suggestionToEdit
  pure $ TitledTextEdit (errorTitle msg) (isUnusedImport msg) edit [] []

isUnusedImport :: ErrorMessage -> Bool
isUnusedImport (ErrorMessage _hints (Errors.UnusedImport {})) = True
isUnusedImport (ErrorMessage _hints (Errors.UnusedExplicitImport {})) = True
isUnusedImport (ErrorMessage _hints (Errors.UnusedDctorImport {})) = True
isUnusedImport (ErrorMessage _hints (Errors.UnusedDctorExplicitImport {})) = True
isUnusedImport _ = False

errorTitle :: ErrorMessage -> Text
errorTitle msg = case Errors.unwrapErrorMessage msg of
  Errors.UnusedImport {} -> "Remove unused import"
  Errors.DuplicateImport {} -> "Remove duplicate import"
  Errors.UnusedExplicitImport {} -> "Remove unused explicit import"
  Errors.UnusedDctorImport {} -> "Remove unused data constructor import"
  Errors.UnusedDctorExplicitImport {} -> "Remove unused data constructor explicit import"
  Errors.ImplicitImport {} -> "Make implicit import explicit"
  Errors.ImplicitQualifiedImport {} -> "Make implicit qualified import explicit"
  Errors.ImplicitQualifiedImportReExport {} -> "Make implicit qualified import re-export explicit"
  Errors.HidingImport {} -> "Address hidden import"
  Errors.MissingTypeDeclaration {} -> "Add missing type declaration"
  Errors.MissingKindDeclaration {} -> "Add missing kind declaration"
  Errors.WildcardInferredType {} -> "Add wildcard inferred type"
  Errors.WarningParsingCSTModule {} -> "Address parser warning"
  _ -> errorCode msg

suggestionToEdit :: JsonErrors.ErrorSuggestion -> Maybe Types.TextEdit
suggestionToEdit (JsonErrors.ErrorSuggestion replacement (Just JsonErrors.ErrorPosition {..})) =
  let rangeStart = Types.Position (fromIntegral $ startLine - 1) (fromIntegral $ startColumn - 1)
      rangeEnd = Types.Position (fromIntegral $ endLine - 1) (fromIntegral $ endColumn - 1)
   in pure $ Types.TextEdit (Types.Range rangeStart rangeEnd) replacement
suggestionToEdit _ = Nothing

data TitledTextEdit = TitledTextEdit
  { tteTitle :: Text,
    tteIsUnusedImport :: Bool,
    tteEdit :: Types.TextEdit,
    tteAllEdits :: [Types.TextEdit],
    tteImportEdits :: [Types.TextEdit]
  }
  deriving (Show, Eq, Generic, A.ToJSON, A.FromJSON)

addAllEdits :: [Types.TextEdit] -> TitledTextEdit -> TitledTextEdit
addAllEdits edits tte = tte {tteAllEdits = tteAllEdits tte <> edits}

addImportEdits :: [Types.TextEdit] -> TitledTextEdit -> TitledTextEdit
addImportEdits edits tte = if tteIsUnusedImport tte then tte {tteImportEdits = tteImportEdits tte <> edits} else tte