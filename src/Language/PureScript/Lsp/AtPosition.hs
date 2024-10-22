{-# LANGUAGE BlockArguments #-}

module Language.PureScript.Lsp.AtPosition where

import Control.Lens (Field1 (_1), view)
import Data.Text qualified as T
import Language.LSP.Protocol.Types qualified as Types
-- import Language.PureScript.Lsp.Monad (m)

import Language.LSP.Server (MonadLsp)
import Language.PureScript qualified as P
import Language.PureScript.AST.SourcePos (nullSourceSpan)
import Language.PureScript.Lsp.Log (debugLsp)
import Language.PureScript.Lsp.NameType (LspNameType (..))
import Language.PureScript.Lsp.ServerConfig (ServerConfig)
import Language.PureScript.Lsp.State (cachedRebuild)
import Language.PureScript.Lsp.Types (LspEnvironment, OpenFile (..))
import Language.PureScript.Lsp.Util (declsAtLine, posInSpan, sourcePosToPosition)
import Language.PureScript.Types (getAnnForType)
import Protolude

-- data AtPosition = AtPosition
--   { apExpr :: [P.Expr],
--     apBinders :: [P.Binder],
--     apType :: [P.SourceType],
--     apDecl :: Maybe P.Declaration,
--     apImport :: Maybe (P.SourceSpan, P.DeclarationRef),
--     apModuleImport :: Maybe (P.SourceSpan, P.ModuleName)
--   }


-- nullAtPosition :: AtPosition
-- nullAtPosition = AtPosition [] [] [] Nothing Nothing Nothing

-- getAtPosition :: [P.Declaration] -> Types.Position -> AtPosition
-- getAtPosition decls pos@(Types.Position{..}) = case head $ declsAtLine (fromIntegral _line + 1) decls of 
--     Nothing -> nullAtPosition
--     Just decl -> AtPosition
--       { apExpr = getExprsAtPos pos decl,
--         apType = getTypesAtPos pos decl,
--         apDecl = Just decl,
--         apImport = findDeclRefAtPos pos (P.getModuleImports decl) <&> \import' -> (P.declRefSourceSpan import', import'),
--         apModuleImport = find (posInSpan pos . fst) (P.getModuleImports decl) 
--       }

atPosition ::
  forall m.
  ( MonadReader LspEnvironment m,
    MonadLsp ServerConfig m
  ) =>
  m () ->
  (LspNameType -> P.ModuleName -> Text -> m ()) ->
  (P.SourceSpan -> P.ModuleName -> [P.DeclarationRef] -> m ()) ->
  (P.SourceSpan -> P.ModuleName -> m ()) ->
  (FilePath -> P.SourcePos -> m ()) ->
  FilePath ->
  Types.Position ->
  m ()
atPosition nullRes handleDecl handleImportRef handleModule handleExprInModule filePath pos@(Types.Position {..}) = do
  cacheOpenMb <- cachedRebuild filePath
  forLsp cacheOpenMb \OpenFile {..} -> do
    let withoutPrim =
          ofModule
            & P.getModuleDeclarations
            & filter (not . isPrimImport)
    handleDecls withoutPrim
  where
    forLsp :: Maybe a -> (a -> m ()) -> m ()
    forLsp a f = maybe nullRes f a

    handleDecls :: [P.Declaration] -> m ()
    handleDecls decls = do
      let srcPosLine = fromIntegral _line + 1

          declsAtPos =
            decls
              & declsAtLine srcPosLine

      forLsp (head declsAtPos) $ \decl -> do
        case decl of
          P.ImportDeclaration (ss, _) importedModuleName importType _ -> do
            debugLsp $ "ImportDeclaration iomportedModuleName: " <> show importedModuleName
            case importType of
              P.Implicit -> handleModule ss importedModuleName
              P.Explicit imports -> handleImportRef ss importedModuleName imports
              P.Hiding imports -> handleImportRef ss importedModuleName imports
          P.TypeInstanceDeclaration _ (P.SourceSpan span start end, _) _ _ _ constraints (P.Qualified (P.ByModuleName modName) className) _args body
            | posInSpan pos classNameSS -> handleDecl TyClassNameType modName classNameTxt
            | Just (P.Constraint _ (P.Qualified (P.ByModuleName conModName) conClassName) _ _ _) <- find (posInSpan pos . fst . P.constraintAnn) constraints -> do
                handleDecl TyClassNameType conModName $ P.runProperName conClassName
            | P.ExplicitInstance members <- body -> do
                handleDecls members
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
                        P.TypeConstructor _ (P.Qualified (P.BySourcePos srcPos) _) | srcPos /= P.SourcePos 0 0 -> handleExprInModule filePath srcPos
                        P.TypeConstructor _ (P.Qualified (P.ByModuleName modName) ident) -> do
                          handleDecl TyNameType modName $ P.runProperName ident
                        P.TypeOp _ (P.Qualified (P.BySourcePos srcPos) _) | srcPos /= P.SourcePos 0 0 -> handleExprInModule filePath srcPos
                        P.TypeOp _ (P.Qualified (P.ByModuleName modName) ident) -> do
                          handleDecl TyOpNameType modName $ P.runOpName ident
                        P.ConstrainedType _ c _ -> case P.constraintClass c of
                          (P.Qualified (P.BySourcePos srcPos) _) -> handleExprInModule filePath srcPos
                          (P.Qualified (P.ByModuleName modName) ident) -> do
                            handleDecl TyClassNameType modName $ P.runProperName ident
                        P.TypeVar _ name -> case findForallSpan name tipes of
                          Just srcSpan -> handleExprInModule filePath (P.spanStart srcSpan)
                          _ -> nullRes
                        _ -> nullRes

                exprsAtPos = getExprsAtPos pos =<< declsAtPos
            debugLsp $ "exprsAtPos: " <> show (length exprsAtPos)
            case smallestExpr exprsAtPos of
              Just expr -> do
                case expr of
                  P.Var _ (P.Qualified (P.BySourcePos srcPos) _) | srcPos /= P.SourcePos 0 0 -> do
                    debugLsp $ "Var BySourcePos : " <> show srcPos
                    handleExprInModule filePath srcPos
                  P.Var _ (P.Qualified (P.ByModuleName modName) ident) -> do
                    debugLsp $ "Var ByModuleName : " <> show modName <> "." <> P.runIdent ident
                    handleDecl IdentNameType modName $ P.runIdent ident
                  P.Op _ (P.Qualified (P.BySourcePos srcPos) _) | srcPos /= P.SourcePos 0 0 -> handleExprInModule filePath srcPos
                  P.Op _ (P.Qualified (P.ByModuleName modName) ident) -> do
                    handleDecl ValOpNameType modName $ P.runOpName ident
                  P.Constructor _ (P.Qualified (P.BySourcePos srcPos) _) | srcPos /= P.SourcePos 0 0 -> handleExprInModule filePath srcPos
                  P.Constructor _ (P.Qualified (P.ByModuleName modName) ident) -> do
                    handleDecl DctorNameType modName $ P.runProperName ident
                  _ -> respondWithTypeLocation
              _ -> respondWithTypeLocation

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

getImportRefNameType :: P.DeclarationRef -> LspNameType
getImportRefNameType = \case
  P.TypeClassRef _ _ -> TyClassNameType
  P.TypeRef _ _ _ -> TyNameType
  P.TypeOpRef _ _ -> TyOpNameType
  P.ValueRef _ _ -> IdentNameType
  P.ValueOpRef _ _ -> ValOpNameType
  P.ModuleRef _ _ -> ModNameType
  P.ReExportRef _ _ _ -> ModNameType
  P.TypeInstanceRef _ _ _ -> IdentNameType

