{-# LANGUAGE BlockArguments #-}

module Language.PureScript.Lsp.Handlers.Definition where

import Control.Lens (Field1 (_1), view, (^.))
import Data.Text qualified as T
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as Message
import Language.LSP.Protocol.Types qualified as Types
import Language.LSP.Server qualified as Server
import Language.PureScript qualified as P
import Language.PureScript.AST.SourcePos (nullSourceSpan)
import Language.PureScript.Lsp.Cache (selectExternPathFromModuleName)
import Language.PureScript.Lsp.Cache.Query (getAstDeclarationLocationInModule)
import Language.PureScript.Lsp.Log (debugLsp)
import Language.PureScript.Lsp.Monad (HandlerM)
import Language.PureScript.Lsp.NameType (LspNameType (..))
import Language.PureScript.Lsp.Print (printName)
import Language.PureScript.Lsp.State (cachedRebuild)
import Language.PureScript.Lsp.Types (OpenFile (..))
import Language.PureScript.Lsp.Util (declsAtLine, posInSpan, sourcePosToPosition)
import Language.PureScript.Types (getAnnForType)
import Protolude hiding (to)

definitionHandler :: Server.Handlers HandlerM
definitionHandler = Server.requestHandler Message.SMethod_TextDocumentDefinition $ \req res -> do
  let Types.DefinitionParams docIdent pos@(Types.Position {..}) _prog _prog' = req ^. LSP.params
      filePathMb = Types.uriToFilePath $ docIdent ^. LSP.uri

      nullRes = res $ Right $ Types.InR $ Types.InR Types.Null

      locationRes fp range = res $ Right $ Types.InL $ Types.Definition $ Types.InL $ Types.Location (Types.filePathToUri fp) range

      posRes fp srcPos = locationRes fp $ Types.Range (sourcePosToPosition srcPos) (sourcePosToPosition srcPos)

      forLsp :: Maybe a -> (a -> HandlerM ()) -> HandlerM ()
      forLsp val f = maybe nullRes f val

      respondWithDeclInOtherModule :: Maybe LspNameType -> P.ModuleName -> Text -> HandlerM ()
      respondWithDeclInOtherModule nameType modName ident = do
        debugLsp $ "respondWithDeclInOtherModule: " <> show nameType <> " " <> show modName <> " " <> show ident
        declSpans <- getAstDeclarationLocationInModule nameType modName ident
        debugLsp $ "SourceSpans: " <> show declSpans
        forLsp (head declSpans) $ \sourceSpan ->
          locationRes (P.spanName sourceSpan) (spanToRange sourceSpan)

      respondWithModule :: P.SourceSpan -> P.ModuleName -> HandlerM ()
      respondWithModule ss modName =
        if posInSpan pos ss
          then do
            modFpMb <- selectExternPathFromModuleName modName
            forLsp modFpMb \modFp -> do
              posRes modFp $ P.SourcePos 1 1
          else nullRes

      respondWithImports ss importedModuleName imports = do
        case findDeclRefAtPos pos imports of
          Just import' -> do
            let name = P.declRefName import'
                nameType = getImportRefNameType import'
            debugLsp $ "import: " <> show import'
            respondWithDeclInOtherModule nameType importedModuleName (printName name)
          _ -> do
            debugLsp $ "respondWithModule importedModuleName: " <> show importedModuleName
            respondWithModule ss importedModuleName

      handleDecls :: FilePath -> [P.Declaration] -> HandlerM ()
      handleDecls filePath decls = do
        let srcPosLine = fromIntegral _line + 1

            declsAtPos =
              decls
                & declsAtLine srcPosLine

        forLsp (head declsAtPos) $ \decl -> do
          case decl of
            P.ImportDeclaration (ss, _) importedModuleName importType _ -> do
              debugLsp $ "ImportDeclaration iomportedModuleName: " <> show importedModuleName
              case importType of
                P.Implicit -> respondWithModule ss importedModuleName
                P.Explicit imports -> respondWithImports ss importedModuleName imports
                P.Hiding imports -> respondWithImports ss importedModuleName imports
            P.TypeInstanceDeclaration _ (P.SourceSpan span start end, _) _ _ _ constraints (P.Qualified (P.ByModuleName modName) className) _args body
              | posInSpan pos classNameSS -> respondWithDeclInOtherModule (Just TyClassNameType) modName classNameTxt
              | Just (P.Constraint _ (P.Qualified (P.ByModuleName conModName) conClassName) _ _ _) <- find (posInSpan pos . fst . P.constraintAnn) constraints -> do
                  respondWithDeclInOtherModule (Just TyClassNameType) conModName $ P.runProperName conClassName
              | P.ExplicitInstance members <- body -> do
                  handleDecls filePath members
              where
                classNameSS = P.SourceSpan span start (P.SourcePos (P.sourcePosLine end) (P.sourcePosColumn start + T.length classNameTxt))

                classNameTxt :: Text
                classNameTxt = P.runProperName className
            -- P.TypeInstanceDeclaration _ _ _ _ _ _ _ -> nullRes
            _ -> do
              let respondWithTypeLocation = do
                    let tipes =
                          filter (not . fromPrim) $
                            filter (not . isNullSourceTypeSpan) $
                              getTypesAtPos pos decl

                    case tipes of
                      [] -> nullRes
                      _ -> do
                        let smallest = minimumBy (comparing getTypeRowsAndColumns) tipes
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

                  exprsAtPos = getExprsAtPos pos =<< declsAtPos
              debugLsp $ "exprsAtPos: " <> show (length exprsAtPos)
              case smallestExpr exprsAtPos of
                Just expr -> do
                  case expr of
                    P.Var _ (P.Qualified (P.BySourcePos srcPos) _) | srcPos /= P.SourcePos 0 0 -> do
                      debugLsp $ "Var BySourcePos : " <> show srcPos
                      posRes filePath srcPos
                    P.Var _ (P.Qualified (P.ByModuleName modName) ident) -> do
                      debugLsp $ "Var ByModuleName : " <> show modName <> "." <> P.runIdent ident
                      respondWithDeclInOtherModule (Just IdentNameType) modName $ P.runIdent ident
                    P.Op _ (P.Qualified (P.BySourcePos srcPos) _) | srcPos /= P.SourcePos 0 0 -> posRes filePath srcPos
                    P.Op _ (P.Qualified (P.ByModuleName modName) ident) -> do
                      respondWithDeclInOtherModule (Just ValOpNameType) modName $ P.runOpName ident
                    P.Constructor _ (P.Qualified (P.BySourcePos srcPos) _) | srcPos /= P.SourcePos 0 0 -> posRes filePath srcPos
                    P.Constructor _ (P.Qualified (P.ByModuleName modName) ident) -> do
                      respondWithDeclInOtherModule (Just DctorNameType) modName $ P.runProperName ident
                    _ -> respondWithTypeLocation
                _ -> respondWithTypeLocation

  forLsp filePathMb \filePath -> do
    cacheOpenMb <- cachedRebuild filePath
    forLsp cacheOpenMb \OpenFile {..} -> do
      let withoutPrim =
            ofModule
              & P.getModuleDeclarations
              & filter (not . isPrimImport)

      handleDecls filePath withoutPrim

smallestExpr :: [P.Expr] -> Maybe P.Expr
smallestExpr [] = Nothing
smallestExpr es = Just $ minimumBy (comparing (fromMaybe (maxInt, maxInt) . getExprRowsAndColumns)) es

getExprRowsAndColumns :: P.Expr -> Maybe (Int, Int)
getExprRowsAndColumns expr =
  P.exprSourceSpan expr <&> \ss ->
    let spanRowStart = P.sourcePosLine (P.spanStart ss)
        spanRowEnd = P.sourcePosLine (P.spanEnd ss)
        spanColStart = P.sourcePosColumn (P.spanStart ss)
        spanColEnd = P.sourcePosColumn (P.spanEnd ss)
     in (spanRowEnd - spanRowStart, spanColEnd - spanColStart)

isNullSourceTypeSpan :: P.SourceType -> Bool
isNullSourceTypeSpan st = getAnnForType st == (nullSourceSpan, [])

isSingleLine :: P.SourceType -> Bool
isSingleLine st = P.sourcePosLine (P.spanStart (fst (getAnnForType st))) == P.sourcePosLine (P.spanEnd (fst (getAnnForType st)))

getTypeRowsAndColumns :: P.SourceType -> (Int, Int)
getTypeRowsAndColumns st = (getTypeRows st, getTypeColumns st)

getTypeColumns :: P.SourceType -> Int
getTypeColumns st = P.sourcePosColumn (P.spanEnd (fst (getAnnForType st))) - P.sourcePosColumn (P.spanStart (fst (getAnnForType st)))

getTypeRows :: P.SourceType -> Int
getTypeRows st = P.sourcePosLine (P.spanEnd (fst (getAnnForType st))) - P.sourcePosLine (P.spanStart (fst (getAnnForType st)))

fromPrim :: P.SourceType -> Bool
fromPrim st = case st of
  P.TypeConstructor _ (P.Qualified (P.ByModuleName (P.ModuleName "Prim")) _) -> True
  P.TypeOp _ (P.Qualified (P.ByModuleName (P.ModuleName "Prim")) _) -> True
  _ -> False

isPrimImport :: P.Declaration -> Bool
isPrimImport (P.ImportDeclaration _ (P.ModuleName "Prim") _ _) = True
isPrimImport (P.ImportDeclaration ss _ _ _) | ss == P.nullSourceAnn = True
isPrimImport _ = False

findForallSpan :: Text -> [P.SourceType] -> Maybe P.SourceSpan
findForallSpan _ [] = Nothing
findForallSpan var (P.ForAll ss _ fa _ _ _ : rest) =
  if fa == var then Just (fst ss) else findForallSpan var rest
findForallSpan var (_ : rest) = findForallSpan var rest

spanToRange :: P.SourceSpan -> Types.Range
spanToRange (P.SourceSpan _ start end) =
  Types.Range
    (sourcePosToPosition start)
    (sourcePosToPosition end)

getExprsAtPos :: Types.Position -> P.Declaration -> [P.Expr]
getExprsAtPos pos declaration = execState (goDecl declaration) []
  where
    goDecl :: P.Declaration -> StateT [P.Expr] Identity P.Declaration
    goDecl = onDecl

    (onDecl, _, _) = P.everywhereOnValuesTopDownM pure handleExpr pure

    handleExpr :: P.Expr -> StateT [P.Expr] Identity P.Expr
    handleExpr expr = do
      when (maybe False (posInSpan pos) (P.exprSourceSpan expr)) do
        modify (expr :)
      pure expr

getTypedValuesAtPos :: Types.Position -> P.Declaration -> [P.Expr]
getTypedValuesAtPos pos declaration = execState (goDecl declaration) []
  where
    goDecl :: P.Declaration -> StateT [P.Expr] Identity P.Declaration
    goDecl = onDecl

    (onDecl, _, _) = P.everywhereOnValuesTopDownM pure handleExpr pure

    handleExpr :: P.Expr -> StateT [P.Expr] Identity P.Expr
    handleExpr expr = do
      case expr of
        P.TypedValue _ e t -> do
          when (maybe False (posInSpan pos) (P.exprSourceSpan e) || posInSpan pos (fst $ getAnnForType t)) do
            modify (expr :)
        _ -> pure ()
      pure expr

getTypesAtPos :: Types.Position -> P.Declaration -> [P.SourceType]
getTypesAtPos pos decl = P.everythingOnTypes (<>) getAtPos =<< (view _1 $ P.accumTypes getAtPos) decl
  where
    getAtPos :: P.SourceType -> [P.SourceType]
    getAtPos st = [st | posInSpan pos (fst $ getAnnForType st)]

findDeclRefAtPos :: (Foldable t) => Types.Position -> t P.DeclarationRef -> Maybe P.DeclarationRef
findDeclRefAtPos pos imports = find (posInSpan pos . P.declRefSourceSpan) imports

getImportRefNameType :: P.DeclarationRef -> Maybe LspNameType
getImportRefNameType = \case
  P.TypeClassRef _ _ -> Just TyClassNameType
  P.TypeRef _ _ _ -> Just TyNameType
  P.TypeOpRef _ _ -> Just TyOpNameType
  P.ValueRef _ _ -> Just IdentNameType
  P.ValueOpRef _ _ -> Just ValOpNameType
  P.ModuleRef _ _ -> Just ModNameType
  P.ReExportRef _ _ _ -> Just ModNameType
  _ -> Nothing