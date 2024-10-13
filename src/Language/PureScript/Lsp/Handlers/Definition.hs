{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Language.PureScript.Lsp.Handlers.Definition where

import Control.Lens (Field1 (_1), view, (^.))
import Control.Lens.Getter (to)
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as Message
import Language.LSP.Protocol.Types qualified as Types
import Language.LSP.Server qualified as Server
import Language.LSP.VFS qualified as VFS
import Language.PureScript (declName)
import Language.PureScript qualified as P
import Language.PureScript.AST qualified as AST
import Language.PureScript.AST.SourcePos (nullSourceSpan)
import Language.PureScript.CoreFn.Expr qualified as CF
import Language.PureScript.Lsp.Cache (selectExternModuleNameFromFilePath, selectExternPathFromModuleName)
import Language.PureScript.Lsp.Cache.Query (getAstDeclarationInModule, getAstDeclarationLocationInModule, getCoreFnExprAt, getEfDeclarationInModule)
import Language.PureScript.Lsp.Docs (readQualifiedNameDocsSourceSpan)
import Language.PureScript.Lsp.Log (debugLsp)
import Language.PureScript.Lsp.Monad (HandlerM)
import Language.PureScript.Lsp.NameType (LspNameType (..))
import Language.PureScript.Lsp.Print (printName)
import Language.PureScript.Lsp.State (cachedRebuild)
import Language.PureScript.Lsp.Types (OpenFile (..))
import Language.PureScript.Lsp.Util (declAtLine, efDeclSourceSpan, getNamesAtPosition, posInSpan, sourcePosToPosition)
import Language.PureScript.Types (getAnnForType)
import Protolude hiding (to)

definitionHandler :: Server.Handlers HandlerM
definitionHandler = Server.requestHandler Message.SMethod_TextDocumentDefinition $ \req res -> do
  let Types.DefinitionParams docIdent pos@(Types.Position {..}) _prog _prog' = req ^. LSP.params
      filePathMb = Types.uriToFilePath $ docIdent ^. LSP.uri

      nullRes = res $ Right $ Types.InR $ Types.InR Types.Null

      locationRes fp range = res $ Right $ Types.InL $ Types.Definition $ Types.InL $ Types.Location (Types.filePathToUri fp) range

      posRes fp srcPos = locationRes fp $ Types.Range (sourcePosToPosition srcPos) (sourcePosToPosition srcPos)

      -- spanRes fp srcSpan = locationRes fp $ spanToRange srcSpan

      forLsp :: Maybe a -> (a -> HandlerM ()) -> HandlerM ()
      forLsp val f = maybe nullRes f val

      respondWithDeclInOtherModule :: Maybe LspNameType -> P.ModuleName -> Text -> HandlerM ()
      respondWithDeclInOtherModule nameType modName ident = do
        declSpans <- getAstDeclarationLocationInModule nameType modName ident
        forLsp (head declSpans) $ \sourceSpan ->
          locationRes (AST.spanName sourceSpan) (spanToRange sourceSpan)

  debugLsp $ "Position: " <> show pos

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

      let declNameAndLine d = (foldMap printName (declName d), P.sourcePosLine $ P.spanStart $ fst $ P.declSourceAnn d)

      debugLsp $ "srcPosLine: " <> show srcPosLine

      debugLsp $ "found decl at pos: " <> maybe "Nothing" (show . declNameAndLine) declAtPos

      forLsp declAtPos $ \decl -> do
        let respondWithTypeLocation = do
              let tipes =
                    filter (not . fromPrim) $
                      filter (not . isNullSourceTypeSpan) $
                        getTypesAtPos pos decl

                  onOneLine = filter isSingleLine tipes
              debugLsp $ "types at pos: " <> show tipes
              case onOneLine of
                [] -> nullRes
                _ -> do
                  let smallest = minimumBy (comparing getTypeColumns) onOneLine
                  debugLsp $ "smallest: " <> show smallest
                  case smallest of
                    P.TypeConstructor _ (P.Qualified (P.BySourcePos srcPos) _) | srcPos /= P.SourcePos 0 0 -> posRes filePath srcPos
                    P.TypeConstructor _ (P.Qualified (P.ByModuleName modName) ident) -> do
                      respondWithDeclInOtherModule (Just TyNameType) modName $ P.runProperName ident
                    P.TypeOp _ (P.Qualified (P.BySourcePos srcPos) _) | srcPos /= P.SourcePos 0 0 -> posRes filePath srcPos
                    P.TypeOp _ (P.Qualified (P.ByModuleName modName) ident) -> do
                      respondWithDeclInOtherModule (Just TyOpNameType) modName $ P.runOpName ident
                    P.ConstrainedType _ c _ -> case P.constraintClass c of
                      (P.Qualified (P.BySourcePos srcPos) _) -> posRes filePath srcPos
                      (P.Qualified (P.ByModuleName modName) ident) -> do
                        respondWithDeclInOtherModule (Just TyClassNameType) modName $ P.runProperName ident
                    P.TypeVar _ name -> case findForallSpan name tipes of
                      Just srcSpan -> posRes filePath (P.spanStart srcSpan)
                      _ -> nullRes
                    _ -> nullRes

            exprsAtPos = getExprsAtPos pos decl

        debugLsp $ "exprs at pos: " <> show (length exprsAtPos)
        case head exprsAtPos of
          Just expr -> do
            case expr of
              P.Var _ (P.Qualified (P.BySourcePos srcPos) _) | srcPos /= P.SourcePos 0 0 -> do
                posRes filePath srcPos
              P.Var _ (P.Qualified (P.ByModuleName modName) ident) -> do
                respondWithDeclInOtherModule (Just IdentNameType) modName $ P.runIdent ident
              P.Op _ (P.Qualified (P.BySourcePos srcPos) _) | srcPos /= P.SourcePos 0 0 -> posRes filePath srcPos
              P.Op _ (P.Qualified (P.ByModuleName srcPos) ident) -> do
                respondWithDeclInOtherModule (Just ValOpNameType) srcPos $ P.runOpName ident
              P.Constructor _ (P.Qualified (P.BySourcePos srcPos) _) | srcPos /= P.SourcePos 0 0 -> posRes filePath srcPos
              P.Constructor _ (P.Qualified (P.ByModuleName srcPos) ident) -> do
                respondWithDeclInOtherModule (Just DctorNameType) srcPos $ P.runProperName ident
              _ -> respondWithTypeLocation
          _ -> respondWithTypeLocation

isNullSourceTypeSpan :: P.SourceType -> Bool
isNullSourceTypeSpan st = getAnnForType st == (nullSourceSpan, [])

isSingleLine :: P.SourceType -> Bool
isSingleLine st = P.sourcePosLine (P.spanStart (fst (getAnnForType st))) == P.sourcePosLine (P.spanEnd (fst (getAnnForType st)))

getTypeColumns :: P.SourceType -> Int
getTypeColumns st = P.sourcePosColumn (P.spanEnd (fst (getAnnForType st))) - P.sourcePosColumn (P.spanStart (fst (getAnnForType st)))

fromPrim :: P.SourceType -> Bool
fromPrim st = case st of
  P.TypeConstructor _ (P.Qualified (P.ByModuleName (P.ModuleName "Prim")) _) -> True
  P.TypeOp _ (P.Qualified (P.ByModuleName (P.ModuleName "Prim")) _) -> True
  _ -> False

isPrimImport :: P.Declaration -> Bool
isPrimImport (P.ImportDeclaration _ (P.ModuleName "Prim") _ _) = True
isPrimImport (P.ImportDeclaration ss _ _ _) | ss == AST.nullSourceAnn = True
isPrimImport _ = False

findForallSpan :: Text -> [P.SourceType] -> Maybe P.SourceSpan
findForallSpan _ [] = Nothing
findForallSpan var (P.ForAll ss _ fa _ _ _ : rest) =
  if fa == var then Just (fst ss) else findForallSpan var rest
findForallSpan var (_ : rest) = findForallSpan var rest

spanToRange :: AST.SourceSpan -> Types.Range
spanToRange (AST.SourceSpan _ start end) =
  Types.Range
    (sourcePosToPosition start)
    (sourcePosToPosition end)

getExprsAtPos :: Types.Position -> P.Declaration -> [P.Expr]
getExprsAtPos pos declaration = execState (goDecl declaration) []
  where
    goDecl :: P.Declaration -> StateT [P.Expr] Identity P.Declaration
    goDecl = onDecl

    (onDecl, _, _) = P.everywhereOnValuesTopDownM pure handleExpr pure

    handleExpr :: AST.Expr -> StateT [P.Expr] Identity AST.Expr
    handleExpr expr = do
      when (maybe False (posInSpan pos) (P.exprSourceSpan expr)) do
        modify (expr :)
      pure expr

getTypesAtPos :: Types.Position -> P.Declaration -> [P.SourceType]
getTypesAtPos pos decl = P.everythingOnTypes (<>) getAtPos =<< (view _1 $ P.accumTypes getAtPos) decl
  where
    getAtPos :: P.SourceType -> [P.SourceType]
    getAtPos st = [st | posInSpan pos (fst $ getAnnForType st)]

definitionHandlerV1 :: Server.Handlers HandlerM
definitionHandlerV1 = Server.requestHandler Message.SMethod_TextDocumentDefinition $ \req res -> do
  let Types.DefinitionParams docIdent pos _prog _prog' = req ^. LSP.params
      filePathMb = Types.uriToFilePath $ docIdent ^. LSP.uri
      uri :: Types.NormalizedUri
      uri =
        req
          ^. LSP.params
            . LSP.textDocument
            . LSP.uri
            . to Types.toNormalizedUri

      nullRes = res $ Right $ Types.InR $ Types.InR Types.Null

      locationRes fp range = res $ Right $ Types.InL $ Types.Definition $ Types.InL $ Types.Location (Types.filePathToUri fp) range

      forLsp :: Maybe a -> (a -> HandlerM ()) -> HandlerM ()
      forLsp val f = maybe nullRes f val
  forLsp filePathMb \filePath -> do
    vfMb <- Server.getVirtualFile uri
    forLsp vfMb \vf -> do
      mNameMb <- selectExternModuleNameFromFilePath filePath
      forLsp mNameMb \mName -> do
        names <- getNamesAtPosition pos mName (VFS._file_text vf)

        case head names of
          Just name -> do
            spanMb <- readQualifiedNameDocsSourceSpan name
            case spanMb of
              _ -> do
                case name of
                  P.Qualified (P.BySourcePos pos') _ -> do
                    locationRes filePath (Types.Range (sourcePosToPosition pos') (sourcePosToPosition pos'))
                  P.Qualified (P.ByModuleName nameModule) ident -> do
                    declMb <- getAstDeclarationInModule nameModule (printName ident)
                    forLsp declMb \decl -> do
                      modFpMb <- selectExternPathFromModuleName nameModule
                      forLsp modFpMb \modFp -> do
                        let sourceSpan = P.declSourceSpan decl
                        locationRes modFp (spanToRange sourceSpan)
              Just span ->
                locationRes (P.spanName span) (spanToRange span)
          _ -> do
            corefnExprMb <- getCoreFnExprAt filePath pos
            case corefnExprMb of
              Just (CF.Var (_ss, _comments, _meta) (P.Qualified qb ident)) -> do
                let name = P.runIdent ident
                case qb of
                  P.ByModuleName coreMName -> do
                    declMb <- getEfDeclarationInModule coreMName name
                    forLsp declMb \decl -> do
                      modFpMb <- selectExternPathFromModuleName coreMName
                      forLsp modFpMb \modFp -> do
                        let sourceSpan = efDeclSourceSpan decl
                        locationRes modFp (spanToRange sourceSpan)
                  P.BySourcePos srcPos ->
                    locationRes filePath (Types.Range (sourcePosToPosition srcPos) (sourcePosToPosition srcPos))
              _ -> nullRes