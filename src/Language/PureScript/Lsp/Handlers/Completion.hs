{-# LANGUAGE BlockArguments #-}

module Language.PureScript.Lsp.Handlers.Completion where

import Control.Lens ((^.))
import Control.Lens.Getter (to)
import Control.Lens.Setter (set)
import Data.Aeson qualified as A
import Data.Text qualified as T
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as Message
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Protocol.Types qualified as Types
import Language.LSP.Server qualified as Server
import Language.LSP.VFS qualified as VFS
import Language.PureScript qualified as P
import Language.PureScript.Ide.Imports (Import (..))
import Language.PureScript.Lsp.Cache.Query (CompletionResult (crModule, crName, crNameType, crType), getAstDeclarationsStartingWith, getAstDeclarationsStartingWithAndSearchingModuleNames, getAstDeclarationsStartingWithOnlyInModule)
import Language.PureScript.Lsp.Docs (readDeclarationDocsWithNameType)
import Language.PureScript.Lsp.Imports (addImportToTextEdit, getIdentModuleQualifier, getMatchingImport, parseModuleNameFromFile)
import Language.PureScript.Lsp.Log (debugLsp, logPerfStandard)
import Language.PureScript.Lsp.Monad (HandlerM)
import Language.PureScript.Lsp.NameType (LspNameType (..), readableType, readableTypeIn)
import Language.PureScript.Lsp.ServerConfig (getMaxCompletions)
import Language.PureScript.Lsp.Types (CompleteItemData (CompleteItemData), decodeCompleteItemData)
import Language.PureScript.Lsp.Util (getSymbolAt)
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
            let (range, word) = getSymbolAt (VFS._file_text vf) pos
            debugLsp $ "Range: " <> show range
            debugLsp $ "Word: " <> word
            mNameMb <- parseModuleNameFromFile uri
            debugLsp $ "mNameMb: " <> show mNameMb
            forLsp mNameMb \mName -> do
              let withQualifier = getIdentModuleQualifier word
                  wordWithoutQual = maybe word snd withQualifier
              limit <- getMaxCompletions
              matchingImport <- maybe (pure Nothing) (getMatchingImport uri . fst) withQualifier
              decls <- logPerfStandard "get completion declarations" case (matchingImport, withQualifier) of
                (Just (Import importModuleName _ _), _) -> do
                  getAstDeclarationsStartingWithOnlyInModule importModuleName wordWithoutQual
                (_, Just (wordModuleName, _)) -> do
                  getAstDeclarationsStartingWithAndSearchingModuleNames mName wordModuleName wordWithoutQual
                _ -> do
                  getAstDeclarationsStartingWith mName wordWithoutQual
              res $
                Right $
                  Types.InR $
                    Types.InL $
                      Types.CompletionList (length decls >= limit) Nothing $
                        decls <&> \cr ->
                          let label = crName cr
                              nameType = crNameType cr
                              declModName = crModule cr
                           in Types.CompletionItem
                                { _label = label
                                , _labelDetails =
                                    Just $
                                      Types.CompletionItemLabelDetails
                                        (Just $ " " <> crType cr)
                                        (Just $ readableTypeIn (crNameType cr) <> P.runModuleName declModName)
                                , _kind =
                                    Just case nameType of
                                      IdentNameType | "->" `T.isInfixOf` crType cr -> Types.CompletionItemKind_Function
                                      IdentNameType -> Types.CompletionItemKind_Value
                                      TyNameType -> Types.CompletionItemKind_Class
                                      DctorNameType -> Types.CompletionItemKind_Constructor
                                      TyClassNameType -> Types.CompletionItemKind_Interface
                                      ValOpNameType -> Types.CompletionItemKind_Operator
                                      TyOpNameType -> Types.CompletionItemKind_TypeParameter
                                      ModNameType -> Types.CompletionItemKind_Module
                                      KindNameType -> Types.CompletionItemKind_Struct
                                      RoleNameType -> Types.CompletionItemKind_Struct
                                , _tags = Nothing
                                , _detail = Nothing
                                , _documentation = Nothing
                                , _deprecated = Nothing --  Maybe Bool
                                , _preselect = Nothing --  Maybe Bool
                                , _sortText = Nothing --  Maybe Text
                                , _filterText = Nothing --  Maybe Text
                                , _insertText = Nothing --  Maybe Text
                                , _insertTextFormat = Nothing --  Maybe Types.InsertTextFormat
                                , _insertTextMode = Nothing --  Maybe Types.InsertTextMode
                                , _textEdit = Just $ Types.InL $ Types.TextEdit range label
                                , _textEditText = Nothing --  Maybe Text
                                , _additionalTextEdits = Nothing --  Maybe [Types.TextEdit]
                                , _commitCharacters = Nothing --  Maybe [Text]
                                , _command = Nothing --  Maybe Types.Command
                                , _data_ = Just $ A.toJSON $ Just $ CompleteItemData filePath mName declModName label nameType word range
                                }
    , Server.requestHandler Message.SMethod_CompletionItemResolve $ \req res -> do
        let completionItem = req ^. LSP.params
            result = completionItem ^. LSP.data_ & decodeCompleteItemData

        case result of
          A.Success (Just cid@(CompleteItemData _filePath _mName declModule label nameType _ _)) -> do
            docsMb <- readDeclarationDocsWithNameType declModule nameType label
            withImports <- addImportToTextEdit completionItem cid
            let setDocs docs = set LSP.documentation (Just $ Types.InR $ Types.MarkupContent Types.MarkupKind_Markdown docs)

                addDocs :: Types.CompletionItem -> Types.CompletionItem
                addDocs =
                  docsMb & maybe
                    (setDocs $ readableType nameType <> " in " <> P.runModuleName declModule)
                    \docs ->
                      setDocs (readableType nameType <> " in " <> P.runModuleName declModule <> "\n\n" <> docs)
            res $
              Right $
                withImports
                  & addDocs
          _ -> res $ Right completionItem
    ]
