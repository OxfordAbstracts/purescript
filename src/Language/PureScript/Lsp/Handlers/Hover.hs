{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Language.PureScript.Lsp.Handlers.Hover where

import Control.Lens ((^.))
import Control.Lens.Getter (to)
import Data.Set qualified as Set
import Data.Text qualified as T
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as Message
import Language.LSP.Protocol.Types qualified as Types
import Language.LSP.Server qualified as Server
import Language.LSP.VFS qualified as VFS
import Language.PureScript qualified as P
import Language.PureScript.AST.Declarations (Expr (..))
import Language.PureScript.AST.Traversals (everythingWithContextOnValues)
import Language.PureScript.CoreFn.Expr qualified as CF
import Language.PureScript.CoreFn.Module (Module (moduleComments))
import Language.PureScript.Docs.Convert.Single (convertComments)
import Language.PureScript.Docs.Types qualified as Docs
import Language.PureScript.Ide.Error (prettyPrintTypeSingleLine)
import Language.PureScript.Lsp.Cache (selectExternModuleNameFromFilePath)
import Language.PureScript.Lsp.Cache.Query (getAstDeclarationLocationInModule, getAstDeclarationTypeInModule, getCoreFnExprAt, getEfDeclarationInModule)
import Language.PureScript.Lsp.Docs (readDeclarationDocsAsMarkdown, readDeclarationDocsWithNameType, readModuleDocs, readQualifiedNameDocsAsMarkdown)
import Language.PureScript.Lsp.Handlers.Definition (findDeclRefAtPos, fromPrim, getExprsAtPos, getImportRefNameType, getTypeColumns, getTypedValuesAtPos, getTypesAtPos, isNullSourceTypeSpan, isPrimImport, isSingleLine, spanToRange)
import Language.PureScript.Lsp.Log (debugLsp)
import Language.PureScript.Lsp.Monad (HandlerM)
import Language.PureScript.Lsp.NameType (LspNameType (..))
import Language.PureScript.Lsp.Print (printName)
import Language.PureScript.Lsp.State (cacheRebuild, cachedRebuild)
import Language.PureScript.Lsp.Types (OpenFile (..))
import Language.PureScript.Lsp.Util (declAtLine, declStartLine, efDeclSourceType, getNamesAtPosition, lookupTypeInEnv)
import Language.PureScript.Names (Qualified (..), disqualify, runIdent)
import Language.PureScript.Types (getAnnForType)
import Protolude hiding (to)

hoverHandler :: Server.Handlers HandlerM
hoverHandler = Server.requestHandler Message.SMethod_TextDocumentHover $ \req res -> do
  let Types.HoverParams docIdent pos@(Types.Position {..}) _prog = req ^. LSP.params
      filePathMb = Types.uriToFilePath $ docIdent ^. LSP.uri

      nullRes = res $ Right $ Types.InR Types.Null

      markdownRes md range = res $ Right $ Types.InL $ Types.Hover (Types.InL $ Types.MarkupContent Types.MarkupKind_Markdown md) range

      forLsp :: Maybe a -> (a -> HandlerM ()) -> HandlerM ()
      forLsp val f = maybe nullRes f val

      respondWithDeclInModule :: P.SourceSpan -> LspNameType -> P.ModuleName -> Text -> HandlerM ()
      respondWithDeclInModule ss nameType modName ident = do
        declDocMb <- readDeclarationDocsWithNameType modName nameType ident
        debugLsp $ "found docs: " <> show (isJust declDocMb)
        case declDocMb of
          Just docs -> markdownRes docs (Just $ spanToRange ss)
          _ -> do
            tipes <- getAstDeclarationTypeInModule (Just nameType) modName ident
            forLsp (head tipes) \tipe ->
              markdownRes tipe (Just $ spanToRange ss)

      respondWithSourceType :: P.Expr -> (P.SourceType, Maybe P.SourceSpan) -> HandlerM ()
      respondWithSourceType expr (tipe, sa) = do
        let word = case expr of
              P.Var _ (P.Qualified _ ident) -> P.runIdent ident
              P.Op _ (P.Qualified _ ident) -> P.runOpName ident
              P.Constructor _ (P.Qualified _ ident) -> P.runProperName ident
              _ -> "<expression>"
            printedType = prettyPrintTypeSingleLine tipe

        markdownRes (pursTypeStr word (Just printedType) []) (spanToRange <$> sa)

      respondWithModule :: P.SourceSpan -> P.ModuleName -> HandlerM ()
      respondWithModule ss modName = do
        docsMb <- readModuleDocs modName
        case docsMb of
          Just docs | Just comments <- Docs.modComments docs -> markdownRes comments (Just $ spanToRange ss)
          _ -> nullRes

      respondWithImports ss importedModuleName imports = do
        case findDeclRefAtPos pos imports of
          Just import' -> do
            let name = P.declRefName import'
                nameType = getImportRefNameType import'
            forLsp nameType \nameType' -> do
              respondWithDeclInModule ss nameType' importedModuleName (printName name)
          _ -> respondWithModule ss importedModuleName

  forLsp filePathMb \filePath -> do
    cacheOpenMb <- cachedRebuild filePath
    forLsp cacheOpenMb \OpenFile {..} -> do
      let withoutPrim =
            ofModule
              & P.getModuleDeclarations
              & filter (not . isPrimImport)

          srcPosLine = fromIntegral _line + 1

          declAtPos =
            withoutPrim
              & declAtLine srcPosLine

      forLsp declAtPos $ \decl -> do
        case decl of
          P.ImportDeclaration (ss, _) importedModuleName importType _ -> do
            case importType of
              P.Implicit -> respondWithModule ss importedModuleName
              P.Explicit imports -> respondWithImports ss importedModuleName imports
              P.Hiding imports -> respondWithImports ss importedModuleName imports
          _ -> do
            debugLsp $ "Decl at pos: " <> show decl
            let exprsAtPos = getExprsAtPos pos decl
                findTypedExpr :: [Expr] -> Maybe (P.SourceType, Maybe P.SourceSpan)
                findTypedExpr ((P.TypedValue _ e t) : _) = Just (t, P.exprSourceSpan e)
                findTypedExpr (_ : es) = findTypedExpr es
                findTypedExpr [] = Nothing

            debugLsp $ "Exprs at pos: " <> show (length exprsAtPos)

            case head exprsAtPos of
              Just expr -> do
                debugLsp $ "found hover expr at pos: " <> show expr
                case expr of
                  P.Var ss (P.Qualified (P.ByModuleName modName) ident) -> do
                    respondWithDeclInModule ss IdentNameType modName (P.runIdent ident)
                  P.Op ss (P.Qualified (P.ByModuleName modName) ident) -> do
                    respondWithDeclInModule ss ValOpNameType modName (P.runOpName ident)
                  P.Constructor ss (P.Qualified (P.ByModuleName modName) ident) -> do
                    respondWithDeclInModule ss DctorNameType modName (P.runProperName ident)
                  _ -> forLsp (findTypedExpr $ getTypedValuesAtPos pos decl) (respondWithSourceType expr)
              _ -> do
                let tipes =
                      filter (not . fromPrim) $
                        filter (not . isNullSourceTypeSpan) $
                          getTypesAtPos pos decl

                    onOneLine = filter isSingleLine tipes
                case onOneLine of
                  [] -> nullRes
                  _ -> do
                    let smallest = minimumBy (comparing getTypeColumns) onOneLine
                    case smallest of
                      P.TypeConstructor (ss, _) (P.Qualified (P.ByModuleName modName) ident) -> do
                        respondWithDeclInModule ss TyNameType modName $ P.runProperName ident
                      P.TypeOp (ss, _) (P.Qualified (P.ByModuleName modName) ident) -> do
                        respondWithDeclInModule ss TyOpNameType modName $ P.runOpName ident
                      P.ConstrainedType (ss, _) c _ -> case P.constraintClass c of 
                        (P.Qualified (P.ByModuleName modName) ident) -> do
                          respondWithDeclInModule ss TyClassNameType modName $ P.runProperName ident
                        _ -> nullRes 
                      _ -> nullRes

hoverHandlerV1 :: Server.Handlers HandlerM
hoverHandlerV1 =
  Server.requestHandler Message.SMethod_TextDocumentHover $ \req res -> do
    let Types.HoverParams docIdent pos _workDone = req ^. LSP.params
        filePathMb = Types.uriToFilePath $ docIdent ^. LSP.uri
        docUri =
          docIdent
            ^. LSP.uri
              . to Types.toNormalizedUri
        nullRes = res $ Right $ Types.InR Types.Null

        markdownRes :: Text -> HandlerM ()
        markdownRes md = res $ Right $ Types.InL $ Types.Hover (Types.InL $ Types.MarkupContent Types.MarkupKind_Markdown md) Nothing

        markdownTypeRes :: Text -> Maybe Text -> [P.Comment] -> HandlerM ()
        markdownTypeRes word type' comments =
          markdownRes $ pursTypeStr word type' comments

        forLsp :: Maybe a -> (a -> HandlerM ()) -> HandlerM ()
        forLsp val f = maybe nullRes f val

    forLsp filePathMb \filePath -> do
      openFileMb <- cachedRebuild filePath
      forLsp openFileMb \_ -> do
        corefnExprMb <- getCoreFnExprAt filePath pos
        case corefnExprMb of
          Just (CF.Literal _ _) -> nullRes
          Just (CF.Constructor (ss, comments, _meta) tName cMame _) -> do
            docsMb <- do
              mNameMb <- selectExternModuleNameFromFilePath (P.spanName ss)
              maybe (pure Nothing) (`readDeclarationDocsAsMarkdown` P.runProperName tName) mNameMb
            case docsMb of
              Nothing -> markdownTypeRes (P.runProperName cMame) (Just $ P.runProperName tName) comments
              Just docs -> markdownRes docs
          Just (CF.Var (_ss, comments, _meta) (P.Qualified qb ident)) -> do
            case qb of
              P.ByModuleName mName -> do
                docsMb <- readDeclarationDocsAsMarkdown mName (P.runIdent ident)
                case docsMb of
                  Just docs -> markdownRes docs
                  _ -> do
                    declMb <- getEfDeclarationInModule mName (runIdent ident)
                    markdownTypeRes (P.runIdent ident) (prettyPrintTypeSingleLine . efDeclSourceType <$> declMb) comments
              P.BySourcePos _pos' ->
                markdownTypeRes (P.runIdent ident) Nothing []
          _ -> do
            vfMb <- Server.getVirtualFile docUri
            forLsp vfMb \vf -> do
              mNameMb <- selectExternModuleNameFromFilePath filePath
              forLsp mNameMb \mName -> do
                names <- getNamesAtPosition pos mName (VFS._file_text vf)
                forLsp (head names) \name -> do
                  docsMb <- readQualifiedNameDocsAsMarkdown name
                  case docsMb of
                    Nothing -> do
                      typeMb <- lookupTypeInEnv filePath name
                      forLsp typeMb \t -> markdownTypeRes (printName $ disqualify name) (Just $ prettyPrintTypeSingleLine t) []
                    Just docs -> markdownRes docs

pursTypeStr :: Text -> Maybe Text -> [P.Comment] -> Text
pursTypeStr word type' comments =
  "```purescript\n"
    <> word
    <> annotation
    <> "\n"
    <> fold (convertComments comments)
    <> "\n```"
  where
    annotation = case type' of
      Just t -> " :: " <> t
      Nothing -> ""

pursMd :: Text -> Text
pursMd t = "```purescript\n" <> t <> "\n```"