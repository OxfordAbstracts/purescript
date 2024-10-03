module Language.PureScript.Lsp.Imports where

import Control.Lens (set)
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Types (CompletionItem, TextEdit)
import Language.PureScript.AST.Declarations qualified as P
import Language.PureScript.Lsp.Types (CompleteItemData (..))
import Language.PureScript.Names qualified as P
import Protolude

addImportToTextEdit :: (MonadIO m) => CompletionItem -> CompleteItemData -> m CompletionItem
addImportToTextEdit completionItem completeItemData = do
  importEdits <- getImportEdits completeItemData
  pure $ set LSP.additionalTextEdits importEdits completionItem

getImportEdits :: (MonadIO m) => CompleteItemData -> m (Maybe [TextEdit])
getImportEdits (CompleteItemData path moduleName' importedModuleName decl) = do
  undefined
