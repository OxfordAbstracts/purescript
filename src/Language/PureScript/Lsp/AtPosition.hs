{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Language.PureScript.Lsp.AtPosition where

import Control.Lens (At, Field1 (_1), Field2 (_2), Field3 (_3), un, view)
-- import Language.PureScript.Lsp.Monad (m)

import Data.List qualified as List
import Data.Text qualified as T
import GHC.IO (unsafePerformIO)
import Language.LSP.Protocol.Types qualified as Types
import Language.LSP.Server (MonadLsp)
import Language.PureScript qualified as P
import Language.PureScript.AST.Declarations (declSourceSpan)
import Language.PureScript.AST.SourcePos (nullSourceSpan)
import Language.PureScript.Lsp.Log (debugLsp)
import Language.PureScript.Lsp.NameType (LspNameType (..))
import Language.PureScript.Lsp.ServerConfig (ServerConfig)
import Language.PureScript.Lsp.State (cachedRebuild)
import Language.PureScript.Lsp.Types (LspEnvironment, OpenFile (..))
import Language.PureScript.Lsp.Util (declsAtLine, posInSpan, sourcePosToPosition)
import Language.PureScript.Traversals (defS)
import Language.PureScript.Types (getAnnForType)
import Protolude

data AtPos
  = APExpr P.SourceSpan Bool P.Expr
  | APBinder P.SourceSpan Bool P.Binder
  | APCaseAlternative P.SourceSpan P.CaseAlternative
  | APDoNotationElement P.SourceSpan Bool P.DoNotationElement
  | APGuard P.SourceSpan P.Guard
  | APType P.SourceType
  | APImport P.SourceSpan P.ModuleName P.ImportDeclarationType (Maybe P.DeclarationRef)
  | APDecl P.Declaration

getSmallestAtPos :: EverythingAtPos -> Maybe AtPos
getSmallestAtPos = \case
  EverythingAtPos {apImport = Just import'} ->
    Just $ uncurry4 APImport import'
  EverythingAtPos {apTypes = types}
    | not . null $ types ->
        Just $ APType $ minimumBy (comparing getTypeLinesAndColumns) types
  EverythingAtPos {apBinders = binders}
    | not . null $ binders ->
        Just $ uncurry3 APBinder $ minimumBy (comparing (spanSize . view _1)) binders
  EverythingAtPos {apExprs = exprs}
    | not . null $ exprs ->
        Just $ uncurry3 APExpr $ minimumBy (comparing (spanSize . view _1)) exprs
  EverythingAtPos {apCaseAlternatives = caseAlts}
    | not . null $ caseAlts ->
        Just $ uncurry APCaseAlternative $ minimumBy (comparing (spanSize . view _1)) caseAlts
  EverythingAtPos {apDoNotationElements = doNotElems}
    | not . null $ doNotElems ->
        Just $ uncurry3 APDoNotationElement $ minimumBy (comparing (spanSize . view _1)) doNotElems
  EverythingAtPos {apGuards = guards}
    | not . null $ guards ->
        Just $ uncurry APGuard $ minimumBy (comparing (spanSize . view _1)) guards
  EverythingAtPos {apDecls = decls}
    | not . null $ decls ->
        Just $ APDecl $ minimumBy (comparing (spanSize . declSourceSpan)) decls
  _ -> Nothing

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d

spanSize :: P.SourceSpan -> (Int, Int)
spanSize (P.SourceSpan _ start end) = (P.sourcePosLine end - P.sourcePosLine start, P.sourcePosColumn end - P.sourcePosColumn start)

data EverythingAtPos = EverythingAtPos
  { apDecls :: [P.Declaration],
    apExprs :: [(P.SourceSpan, Bool, P.Expr)],
    apBinders :: [(P.SourceSpan, Bool, P.Binder)],
    apCaseAlternatives :: [(P.SourceSpan, P.CaseAlternative)],
    apDoNotationElements :: [(P.SourceSpan, Bool, P.DoNotationElement)],
    apGuards :: [(P.SourceSpan, P.Guard)],
    apTypes :: [P.SourceType],
    apImport :: Maybe (P.SourceSpan, P.ModuleName, P.ImportDeclarationType, Maybe P.DeclarationRef)
  }
  deriving (Show)

nullEverythingAtPos :: EverythingAtPos
nullEverythingAtPos = EverythingAtPos [] [] [] [] [] [] [] Nothing

withSpansOnly :: EverythingAtPos -> EverythingAtPos
withSpansOnly EverythingAtPos {..} =
  EverythingAtPos
    apDecls
    (filter (view _2) apExprs)
    (filter (view _2) apBinders)
    []
    (filter (view _2) apDoNotationElements)
    []
    apTypes
    apImport

withTypedValuesOnly :: EverythingAtPos -> EverythingAtPos
withTypedValuesOnly EverythingAtPos {..} =
  EverythingAtPos
    apDecls
    (filter (isJust . exprTypes . view _3) apExprs)
    (filter (isJust . binderTypes . view _3) apBinders)
    []
    []
    []
    apTypes
    apImport
  where
    (_, exprTypes, binderTypes, _, _) =
      P.accumTypes (const $ Just ())

getEverythingAtPos :: [P.Declaration] -> Types.Position -> EverythingAtPos
getEverythingAtPos decls pos@(Types.Position {..}) =
  case head $ declsAtLine (fromIntegral _line + 1) $ filter (not . isPrimImport) decls of
    Nothing -> nullEverythingAtPos
    Just (P.ImportDeclaration (ss, _) importedModuleName importType _) ->
      nullEverythingAtPos {apImport = Just (maybe ss P.declRefSourceSpan ref, importedModuleName, importType, ref)}
      where
        ref = findDeclRefAtPos pos case importType of
          P.Implicit -> []
          P.Explicit refs -> refs
          P.Hiding refs -> refs
    Just topDecl -> execState (handleDecl topDecl) nullEverythingAtPos {apDecls = [topDecl]}
      where
        (handleDecl, _, _, _, _, _) = P.everywhereWithContextOnValuesM (declSourceSpan topDecl) onDecl onExpr onBinder onCaseAlternative onDoNotationElement onGuard

        onDecl :: P.SourceSpan -> P.Declaration -> StateT EverythingAtPos Identity (P.SourceSpan, P.Declaration)
        onDecl _ decl = do
          let ss = declSourceSpan decl
          when (posInSpan pos ss) do
            modify $ addDecl decl
          addTypesSt $ declTypes decl
          pure (ss, decl)

        onExpr ss expr = do
          let ssMb = P.exprSourceSpan expr
              ss' = fromMaybe ss ssMb
          when (posInSpan pos ss' && not (isPlaceholder expr)) do
            modify $ addExpr ss' (isJust ssMb) expr
          addTypesSt $ exprTypes expr
          pure (ss', expr)

        onBinder ss binder = do
          let ssMb = binderSourceSpan binder
              ss' = fromMaybe ss ssMb
          when (posInSpan pos ss') do
            modify $ addBinder ss' (isJust ssMb) binder
          addTypesSt $ binderTypes binder
          pure (ss', binder)

        onCaseAlternative :: P.SourceSpan -> P.CaseAlternative -> StateT EverythingAtPos Identity (P.SourceSpan, P.CaseAlternative)
        onCaseAlternative ss caseAlt = do
          when (posInSpan pos ss) do
            modify $ addCaseAlternative ss caseAlt
          addTypesSt $ caseAltTypes caseAlt
          pure (ss, caseAlt)

        onDoNotationElement :: P.SourceSpan -> P.DoNotationElement -> StateT EverythingAtPos Identity (P.SourceSpan, P.DoNotationElement)
        onDoNotationElement ss doNotationElement = do
          let ssMb = doNotationElementSpan doNotationElement
              ss' = fromMaybe ss ssMb
          when (posInSpan pos ss') do
            modify $ addDoNotationElement ss' (isJust ssMb) doNotationElement
          addTypesSt $ doNotTypes doNotationElement
          pure (ss', doNotationElement)

        onGuard :: P.SourceSpan -> P.Guard -> StateT EverythingAtPos Identity (P.SourceSpan, P.Guard)
        onGuard ss guard' = do
          when (posInSpan pos ss) do
            modify (addGuard ss guard')
          pure (ss, guard')

        binderSourceSpan :: P.Binder -> Maybe P.SourceSpan
        binderSourceSpan = \case
          P.NullBinder -> Nothing
          P.LiteralBinder ss _ -> Just ss
          P.VarBinder ss _ -> Just ss
          P.ConstructorBinder ss _ _ -> Just ss
          P.NamedBinder ss _ _ -> Just ss
          P.PositionedBinder ss _ _ -> Just ss
          P.TypedBinder ss _ -> Just (fst $ getAnnForType ss)
          P.OpBinder ss _ -> Just ss
          P.BinaryNoParensBinder {} -> Nothing
          P.ParensInBinder {} -> Nothing

        doNotationElementSpan :: P.DoNotationElement -> Maybe P.SourceSpan
        doNotationElementSpan = \case
          P.PositionedDoNotationElement ss _ _ -> Just ss
          _ -> Nothing

        (declTypes, exprTypes, binderTypes, caseAltTypes, doNotTypes) = P.accumTypes (getTypesAtPos pos)

        isPlaceholder :: P.Expr -> Bool
        isPlaceholder = \case
          P.TypeClassDictionary {} -> True
          P.DeferredDictionary {} -> True
          P.DerivedInstancePlaceholder {} -> True
          _ -> False

addDecl :: P.Declaration -> EverythingAtPos -> EverythingAtPos
addDecl decl atPos = atPos {apDecls = decl : apDecls atPos}

addExpr :: P.SourceSpan -> Bool -> P.Expr -> EverythingAtPos -> EverythingAtPos
addExpr ss hasOwnSs expr atPos = atPos {apExprs = (ss, hasOwnSs, expr) : apExprs atPos}

addBinder :: P.SourceSpan -> Bool -> P.Binder -> EverythingAtPos -> EverythingAtPos
addBinder ss hasOwnSs binder atPos = atPos {apBinders = (ss, hasOwnSs, binder) : apBinders atPos}

addCaseAlternative :: P.SourceSpan -> P.CaseAlternative -> EverythingAtPos -> EverythingAtPos
addCaseAlternative ss binder atPos = atPos {apCaseAlternatives = (ss, binder) : apCaseAlternatives atPos}

addDoNotationElement :: P.SourceSpan -> Bool -> P.DoNotationElement -> EverythingAtPos -> EverythingAtPos
addDoNotationElement ss hasOwnSs doNotationElement atPos =
  atPos {apDoNotationElements = (ss, hasOwnSs, doNotationElement) : apDoNotationElements atPos}

addGuard :: P.SourceSpan -> P.Guard -> EverythingAtPos -> EverythingAtPos
addGuard ss guard' atPos = atPos {apGuards = (ss, guard') : apGuards atPos}

addTypes :: [P.SourceType] -> EverythingAtPos -> EverythingAtPos
addTypes tys atPos = atPos {apTypes = tys <> apTypes atPos}

addTypesSt :: (MonadState EverythingAtPos m) => [P.SourceType] -> m ()
addTypesSt tys = modify (addTypes tys)

debugExpr :: P.Expr -> Text
debugExpr =
  T.replace ", sourcePosColumn = " ":"
    . T.replace "SourcePos {sourcePosLine = " ""
    . T.replace "SourceSpan {spanEnd = SourcePos {sourcePosLine =  " "end = "
    . T.replace "SourceSpan {spanStart = SourcePos {sourcePosLine =  " "start = "
    . T.replace "spanName = \"/Users/rorycampbell/Documents/projects/simple-purs/src/B.purs\", " ""
    . show

debugSrcSpan :: P.SourceSpan -> Text
debugSrcSpan = 
  T.replace ", sourcePosColumn = " ":"
    . T.replace "SourcePos {sourcePosLine = " ""
    . T.replace "SourceSpan {spanEnd = SourcePos {sourcePosLine =  " "end = "
    . T.replace "SourceSpan {spanStart = SourcePos {sourcePosLine =  " "start = "
    . T.replace "spanName = \"/Users/rorycampbell/Documents/projects/simple-purs/src/B.purs\", " ""
    . show


-- getDeclTypesAtPos :: Types.Position -> P.Declaration -> [P.SourceType]

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
                            getDeclTypesAtPos pos decl

                  case tipes of
                    [] -> nullRes
                    _ -> do
                      let smallest = minimumBy (comparing getTypeLinesAndColumns) tipes
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
smallestExpr es = Just $ minimumBy (comparing (fromMaybe (maxInt, maxInt) . getExprLinesAndColumns)) es

getExprLinesAndColumns :: P.Expr -> Maybe (Int, Int)
getExprLinesAndColumns expr =
  P.exprSourceSpan expr <&> \ss ->
    let spanLineStart = P.sourcePosLine (P.spanStart ss)
        spanLineEnd = P.sourcePosLine (P.spanEnd ss)
        spanColStart = P.sourcePosColumn (P.spanStart ss)
        spanColEnd = P.sourcePosColumn (P.spanEnd ss)
     in (spanLineEnd - spanLineStart, spanColEnd - spanColStart)

isNullSourceTypeSpan :: P.SourceType -> Bool
isNullSourceTypeSpan st = getAnnForType st == (nullSourceSpan, [])

isSingleLine :: P.SourceType -> Bool
isSingleLine st = P.sourcePosLine (P.spanStart (fst (getAnnForType st))) == P.sourcePosLine (P.spanEnd (fst (getAnnForType st)))

getTypeLinesAndColumns :: P.SourceType -> (Int, Int)
getTypeLinesAndColumns st = (getTypeLines st, getTypeColumns st)

getTypeColumns :: P.SourceType -> Int
getTypeColumns st = P.sourcePosColumn (P.spanEnd (fst (getAnnForType st))) - P.sourcePosColumn (P.spanStart (fst (getAnnForType st)))

getTypeLines :: P.SourceType -> Int
getTypeLines st = P.sourcePosLine (P.spanEnd (fst (getAnnForType st))) - P.sourcePosLine (P.spanStart (fst (getAnnForType st)))

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

getDeclTypesAtPos :: Types.Position -> P.Declaration -> [P.SourceType]
getDeclTypesAtPos pos decl = getTypesAtPos pos =<< (view _1 $ P.accumTypes getAtPos) decl
  where
    getAtPos :: P.SourceType -> [P.SourceType]
    getAtPos st = [st | posInSpan pos (fst $ getAnnForType st)]

getTypesAtPos :: Types.Position -> P.SourceType -> [P.SourceType]
getTypesAtPos pos st = P.everythingOnTypes (<>) getAtPos st
  where
    getAtPos :: P.SourceType -> [P.SourceType]
    getAtPos st' = [st' | posInSpan pos (fst $ getAnnForType st')]

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

-- t =
--   EverythingAtPos = Nothing,
--       apDecls =
--         [ ValueDeclaration
--             ( ValueDeclarationData
--                 { valdeclSourceAnn =
--                     ( SourceSpan
--                         { spanStart = SourcePos {sourcePosLine = 62, sourcePosColumn = 1},
--                           spanEnd = SourcePos {sourcePosLine = 62, sourcePosColumn = 15}
--                         },
--                       []
--                     ),
--                   valdeclIdent = Ident "zzzzz",
--                   valdeclName = Public,
--                   valdeclBinders = [],
--                   valdeclExpression =
--                     [ GuardedExpr
--                         []
--                         ( TypedValue
--                             True
--                             ( PositionedValue
--                                 ( SourceSpan
--                                     { spanStart = SourcePos {sourcePosLine = 62, sourcePosColumn = 9},
--                                       spanEnd = SourcePos {sourcePosLine = 62, sourcePosColumn = 15}
--                                     }
--                                 )
--                                 []
--                                 ( Literal
--                                     ( SourceSpan
--                                         { spanStart = SourcePos {sourcePosLine = 62, sourcePosColumn = 9},
--                                           spanEnd = SourcePos {sourcePosLine = 62, sourcePosColumn = 15}
--                                         }
--                                     )
--                                     (NumericLiteral (Left 333333))
--                                 )
--                             )
--                             (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Int"})))
--                         )
--                     ]
--                 }
--             )
--         ],
--       apExprs =
--         [ ( SourceSpan
--               { spanStart = SourcePos {sourcePosLine = 62, sourcePosColumn = 9},
--                 spanEnd = SourcePos {sourcePosLine = 62, sourcePosColumn = 15}
--               },
--             True,
--             Literal
--               ( SourceSpan
--                   { spanStart = SourcePos {sourcePosLine = 62, sourcePosColumn = 9},
--                     spanEnd = SourcePos {sourcePosLine = 62, sourcePosColumn = 15}
--                   }
--               )
--               (NumericLiteral (Left 333333))
--           ),
--           ( SourceSpan
--               { spanStart = SourcePos {sourcePosLine = 62, sourcePosColumn = 1},
--                 spanEnd = SourcePos {sourcePosLine = 62, sourcePosColumn = 15}
--               },
--             True,
--             PositionedValue
--               ( SourceSpan
--                   { spanStart = SourcePos {sourcePosLine = 62, sourcePosColumn = 9},
--                     spanEnd = SourcePos {sourcePosLine = 62, sourcePosColumn = 15}
--                   }
--               )
--               []
--               ( Literal
--                   ( SourceSpan
--                       { spanStart = SourcePos {sourcePosLine = 62, sourcePosColumn = 9},
--                         spanEnd = SourcePos {sourcePosLine = 62, sourcePosColumn = 15}
--                       }
--                   )
--                   (NumericLiteral (Left 333333))
--               )
--           ),
--           ( SourceSpan
--               { spanStart = SourcePos {sourcePosLine = 62, sourcePosColumn = 1},
--                 spanEnd = SourcePos {sourcePosLine = 62, sourcePosColumn = 15}
--               },
--             False,
--             P.TypedValue
--               True
--               ( PositionedValue
--                   ( SourceSpan
--                       { spanStart = SourcePos {sourcePosLine = 62, sourcePosColumn = 9},
--                         spanEnd = SourcePos {sourcePosLine = 62, sourcePosColumn = 15}
--                       }
--                   )
--                   []
--                   ( Literal
--                       ( SourceSpan
--                           { spanStart = SourcePos {sourcePosLine = 62, sourcePosColumn = 9},
--                             spanEnd = SourcePos {sourcePosLine = 62, sourcePosColumn = 15}
--                           }
--                       )
--                       (NumericLiteral (Left 333333))
--                   )
--               )
--               (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Int"})))
--           )
--         ],
--       apBinders = [],
--       apCaseAlternatives = [],
--       apDoNotationElements = [],
--       apGuards = [],
--       apTypes = [],
--       apImport = Nothing
--     }