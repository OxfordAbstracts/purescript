{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.PureScript.Lsp.Handlers.Hover where

import Control.Lens ((^.))
import Data.Text qualified as T
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as Message
import Language.LSP.Protocol.Types qualified as Types
import Language.LSP.Server qualified as Server
import Language.PureScript qualified as P
import Language.PureScript.AST.Declarations (Expr (..))
import Language.PureScript.Docs.Convert.Single (convertComments)
import Language.PureScript.Docs.Types qualified as Docs
import Language.PureScript.Ide.Error (prettyPrintTypeSingleLine)
import Language.PureScript.Lsp.Cache.Query (getAstDeclarationTypeInModule)
import Language.PureScript.Lsp.Docs (readDeclarationDocsWithNameType, readModuleDocs)
import Language.PureScript.Lsp.AtPosition (findDeclRefAtPos, fromPrim, getExprsAtPos, getImportRefNameType, getTypeRowsAndColumns, getTypedValuesAtPos, getTypesAtPos, isNullSourceTypeSpan, isPrimImport, smallestExpr, spanToRange)
import Language.PureScript.Lsp.Log (debugLsp)
import Language.PureScript.Lsp.Monad (HandlerM)
import Language.PureScript.Lsp.NameType (LspNameType (..))
import Language.PureScript.Lsp.Print (printName)
import Language.PureScript.Lsp.State (cachedRebuild)
import Language.PureScript.Lsp.Types (OpenFile (..))
import Language.PureScript.Lsp.Util (declsAtLine, posInSpan)
import Protolude hiding (to)
import Text.PrettyPrint.Boxes (render)

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
              _ -> T.pack $ render $ P.prettyPrintValue 3 expr
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
            respondWithDeclInModule ss nameType importedModuleName (printName name)
          _ -> respondWithModule ss importedModuleName

      handleDecls :: [P.Declaration] -> HandlerM ()
      handleDecls decls = do
        let srcPosLine = fromIntegral _line + 1

            declsAtPos =
              decls
                & declsAtLine srcPosLine

        debugLsp $ "declsAtPos: " <> show (length declsAtPos)

        forLsp (head declsAtPos) $ \decl -> do
          case decl of
            P.ImportDeclaration (ss, _) importedModuleName importType _ -> do
              case importType of
                P.Implicit -> respondWithModule ss importedModuleName
                P.Explicit imports -> respondWithImports ss importedModuleName imports
                P.Hiding imports -> respondWithImports ss importedModuleName imports
            P.TypeInstanceDeclaration _ (P.SourceSpan span start end, _) _ _ _ constraints (P.Qualified (P.ByModuleName modName) className) _args body
              | posInSpan pos classNameSS -> respondWithDeclInModule classNameSS TyClassNameType modName classNameTxt
              | Just (P.Constraint (ss, _) (P.Qualified (P.ByModuleName conModName) conClassName) _ _ _) <- find (posInSpan pos . fst . P.constraintAnn) constraints -> do
                  respondWithDeclInModule ss TyClassNameType conModName $ P.runProperName conClassName
              | P.ExplicitInstance members <- body, not $ null $ declsAtLine srcPosLine members  -> do
                  handleDecls members
              where
                classNameSS = P.SourceSpan span start (P.SourcePos (P.sourcePosLine end) (P.sourcePosColumn start + T.length classNameTxt))

                classNameTxt :: Text
                classNameTxt = P.runProperName className
            _ -> do
              let exprsAtPos = getExprsAtPos pos =<< declsAtPos
                  findTypedExpr :: [Expr] -> Maybe (P.SourceType, Maybe P.SourceSpan)
                  findTypedExpr ((P.TypedValue _ e t) : _) = Just (t, P.exprSourceSpan e)
                  findTypedExpr (_ : es) = findTypedExpr es
                  findTypedExpr [] = Nothing

              debugLsp $ "exprsAtPos: " <> show (length exprsAtPos)

              case smallestExpr exprsAtPos of
                Just expr -> do
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

                  debugLsp $ "tipes: " <> show (length tipes)

                  case tipes of
                    [] -> nullRes
                    _ -> do
                      let smallest = minimumBy (comparing getTypeRowsAndColumns) tipes
                      debugLsp $ "smallest: " <> show smallest
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

  forLsp filePathMb \filePath -> do
    cacheOpenMb <- cachedRebuild filePath
    forLsp cacheOpenMb \OpenFile {..} -> do
      let withoutPrim =
            ofModule
              & P.getModuleDeclarations
              & filter (not . isPrimImport)

      handleDecls withoutPrim

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
