{-# LANGUAGE BlockArguments #-}

module Language.PureScript.Lsp.Handlers.Completion where

import Control.Lens ((^.))
import Control.Lens.Getter (to)
import Control.Lens.Setter (set)
import Data.Aeson qualified as A
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as Message
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Protocol.Types qualified as Types
import Language.LSP.Server qualified as Server
import Language.LSP.VFS qualified as VFS
import Language.PureScript qualified as P
import Language.PureScript.Ide.Imports (Import (..))
import Language.PureScript.Lsp.Cache.Query (CompletionResult (crModule, crName, crType, crNameType), getAstDeclarationsStartingWith, getAstDeclarationsStartingWithAndSearchingModuleNames, getAstDeclarationsStartingWithOnlyInModule)
import Language.PureScript.Lsp.Docs (readDeclarationDocsAsMarkdown)
import Language.PureScript.Lsp.Imports (addImportToTextEdit, getIdentModuleQualifier, getMatchingImport, parseModuleNameFromFile)
import Language.PureScript.Lsp.Log (logPerfStandard, debugLsp)
import Language.PureScript.Lsp.Monad (HandlerM)
import Language.PureScript.Lsp.ServerConfig (getMaxCompletions)
import Language.PureScript.Lsp.Types (CompleteItemData (CompleteItemData), decodeCompleteItemData)
import Language.PureScript.Lsp.Util (getWordAt)
import Protolude hiding (to)

completionAndResolveHandlers :: Server.Handlers HandlerM
completionAndResolveHandlers =
  mconcat
    [ Server.requestHandler Message.SMethod_TextDocumentCompletion $ \req res -> do
        let Types.CompletionParams docIdent pos _prog _prog' _completionCtx = req ^. LSP.params
            filePathMb = Types.uriToFilePath $ docIdent ^. LSP.uri
            uri :: Types.NormalizedUri
            uri =
              req
                ^. LSP.params
                  . LSP.textDocument
                  . LSP.uri
                  . to Types.toNormalizedUri

            nullRes = res $ Right $ Types.InR $ Types.InR Types.Null

            forLsp :: Maybe a -> (a -> HandlerM ()) -> HandlerM ()
            forLsp val f = maybe nullRes f val

        forLsp filePathMb \filePath -> do
          vfMb <- Server.getVirtualFile uri
          forLsp vfMb \vf -> do
            let (range, word) = getWordAt (VFS._file_text vf) pos
            mNameMb <- parseModuleNameFromFile uri
            debugLsp $ "word: " <> show word
            forLsp mNameMb \mName -> do
              let withQualifier = getIdentModuleQualifier word
                  wordWithoutQual = maybe word snd withQualifier
              debugLsp $ "withQualifier: " <> show withQualifier
              limit <- getMaxCompletions
              matchingImport <- maybe (pure Nothing) (getMatchingImport uri . fst) withQualifier
              decls <- logPerfStandard "get completion declarations" case (matchingImport, withQualifier) of
                (Just (Import importModuleName _ _), _) -> do 
                  getAstDeclarationsStartingWithOnlyInModule importModuleName wordWithoutQual
                (_, Just (wordModuleName, _)) -> do 
                  getAstDeclarationsStartingWithAndSearchingModuleNames mName wordModuleName wordWithoutQual
                _ ->  do
                  getAstDeclarationsStartingWith mName wordWithoutQual
              res $
                Right $
                  Types.InR $
                    Types.InL $
                      Types.CompletionList (length decls >= limit) Nothing $
                        decls <&> \cr ->
                          let label = crName cr
                            in Types.CompletionItem
                                { _label = label,
                                  _labelDetails =
                                    Just $
                                      Types.CompletionItemLabelDetails
                                        (Just $ " " <> crType cr)
                                        (Just $ P.runModuleName (crModule cr)),
                                  _kind = Nothing, --  Maybe Types.CompletionItemKind TODO: add kind
                                  _tags = Nothing,
                                  _detail = Nothing,
                                  _documentation = Nothing,
                                  _deprecated = Nothing, --  Maybe Bool
                                  _preselect = Nothing, --  Maybe Bool
                                  _sortText = Nothing, --  Maybe Text
                                  _filterText = Nothing, --  Maybe Text
                                  _insertText = Nothing, --  Maybe Text
                                  _insertTextFormat = Nothing, --  Maybe Types.InsertTextFormat
                                  _insertTextMode = Nothing, --  Maybe Types.InsertTextMode
                                  _textEdit = Nothing, --  Maybe
                                  --                (Types.TextEdit Types.|? Types.InsertReplaceEdit)
                                  _textEditText = Nothing, --  Maybe Text
                                  _additionalTextEdits = Nothing, --  Maybe [Types.TextEdit]
                                  _commitCharacters = Nothing, --  Maybe [Text]
                                  _command = Nothing, --  Maybe Types.Command
                                  _data_ = Just $ A.toJSON $ Just $ CompleteItemData filePath mName (crModule cr) label (crNameType cr) word range
                                },
      Server.requestHandler Message.SMethod_CompletionItemResolve $ \req res -> do
        let completionItem = req ^. LSP.params
            result = completionItem ^. LSP.data_ & decodeCompleteItemData

        case result of
          A.Success (Just cid@(CompleteItemData _filePath _mName declModule label _ _ _)) -> do
            docsMb <- readDeclarationDocsAsMarkdown declModule label
            withImports <- addImportToTextEdit completionItem cid
            let addDocs :: Types.CompletionItem -> Types.CompletionItem
                addDocs =
                  docsMb & maybe
                    identity
                    \docs ->
                      set LSP.documentation (Just $ Types.InR $ Types.MarkupContent Types.MarkupKind_Markdown docs)
            res $
              Right $
                withImports
                  & addDocs
          _ -> res $ Right completionItem
    ]