-- |
-- This module implements the desugaring pass which reapplies binary operators based
-- on their fixity data and removes explicit parentheses.
--
-- The value parser ignores fixity data when parsing binary operator applications, so
-- it is necessary to reorder them here.
module Language.PureScript.Sugar.Operators
  ( desugarSignedLiterals,
    RebracketCaller (..),
    rebracket,
    rebracketFixitiesOnly,
    rebracketFiltered,
    checkFixityExports,
  )
where

import Control.Monad (unless, (<=<))
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Supply.Class (MonadSupply)
import Data.Either (partitionEithers)
import Data.Foldable (for_, traverse_)
import Data.Function (on)
import Data.Functor (($>))
import Data.Functor.Identity (Identity (..), runIdentity)
import Data.List (groupBy, sortOn)
import Data.Map qualified as M
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import Language.PureScript.AST
import Language.PureScript.Constants.Libs qualified as C
import Language.PureScript.Crash (internalError)
import Language.PureScript.Errors (MultipleErrors, SimpleErrorMessage (..), addHint, errorMessage, errorMessage', parU, rethrow, rethrowWithPosition)
import Language.PureScript.Externs (ExternsFile (..), ExternsFixity (..), ExternsTypeFixity (..))
import Language.PureScript.Names (Ident (..), Name (..), OpName, OpNameType (..), ProperName, ProperNameType (..), Qualified (..), QualifiedBy (..), freshIdent', pattern ByNullSourcePos)
import Language.PureScript.Names qualified as P
import Language.PureScript.Sugar.Operators.Binders (matchBinderOperators)
import Language.PureScript.Sugar.Operators.Expr (matchExprOperators)
import Language.PureScript.Sugar.Operators.Types (matchTypeOperators)
import Language.PureScript.Traversals (defS, sndM)
import Language.PureScript.Types (Constraint (..), SourceType, Type (..), everywhereOnTypesTopDownM, overConstraintArgs)
import Prelude

-- |
-- Removes unary negation operators and replaces them with calls to `negate`.
desugarSignedLiterals :: Module -> Module
desugarSignedLiterals (Module ss coms mn ds exts) =
  Module ss coms mn (map f' ds) exts
  where
    (f', _, _) = everywhereOnValues id go id
    go (UnaryMinus ss' val) = App (Var ss' (Qualified ByNullSourcePos (Ident C.S_negate))) val
    go other = other

-- |
-- An operator associated with its declaration position, fixity, and the name
-- of the function or data constructor it is an alias for.
type FixityRecord op alias = (Qualified op, SourceSpan, Fixity, Qualified alias)

type ValueFixityRecord = FixityRecord (OpName 'ValueOpName) (Either Ident (ProperName 'ConstructorName))

type TypeFixityRecord = FixityRecord (OpName 'TypeOpName) (ProperName 'TypeName)

-- |
-- Remove explicit parentheses and reorder binary operator applications.
--
-- This pass requires name desugaring and export elaboration to have run first.
rebracket ::
  forall m.
  (MonadError MultipleErrors m) =>
  (MonadSupply m) =>
  [ExternsFile] ->
  Module ->
  m Module
rebracket =
  rebracketFiltered CalledByCompile (const True)

-- | rebracket that takes the fixities without the other externs fields
rebracketFixitiesOnly ::
  forall m.
  (MonadError MultipleErrors m) =>
  (MonadSupply m) =>
  [(P.ModuleName, [ExternsFixity])] ->
  [(P.ModuleName, [ExternsTypeFixity])] ->
  Module ->
  m Module
rebracketFixitiesOnly exFixities exTypeFixities =
  rebracketFiltered' CalledByCompile (const False) $
    fixities <> typeFixities

  where 
    fixities = concatMap (\(mName, fs) -> fmap (fromFixity mName) fs) exFixities
    typeFixities = concatMap (\(mName, fs) -> fmap (fromTypeFixity mName) fs) exTypeFixities
      -- >>= \(name, fs, tFs) ->
      --   externsFixities' name fs tFs

-- |
-- A version of `rebracket` which allows you to choose which declarations
-- should be affected. This is used in docs generation, where we want to
-- desugar type operators in instance declarations to ensure that instances are
-- paired up with their types correctly, but we don't want to desugar type
-- operators in value declarations.
rebracketFiltered ::
  forall m.
  (MonadError MultipleErrors m) =>
  (MonadSupply m) =>
  RebracketCaller ->
  (Declaration -> Bool) ->
  [ExternsFile] ->
  Module ->
  m Module
rebracketFiltered !caller pred_ externs m = do
  rebracketFiltered' caller pred_ (concatMap externsFixities externs) m

rebracketFiltered' ::
  forall m.
  (MonadError MultipleErrors m) =>
  (MonadSupply m) =>
  RebracketCaller ->
  (Declaration -> Bool) ->
  [Either ValueFixityRecord TypeFixityRecord] ->
  Module ->
  m Module
rebracketFiltered' !caller pred_ fixities m = do
  let (valueFixities, typeFixities) =
        partitionEithers $
          fixities
            ++ collectFixities m

  ensureNoDuplicates' MultipleValueOpFixities valueFixities
  ensureNoDuplicates' MultipleTypeOpFixities typeFixities

  let valueOpTable = customOperatorTable' valueFixities
  let valueAliased = M.fromList (map makeLookupEntry valueFixities)
  let typeOpTable = customOperatorTable' typeFixities
  let typeAliased = M.fromList (map makeLookupEntry typeFixities)

  rebracketModule caller pred_ valueOpTable typeOpTable m
    >>= renameAliasedOperators valueAliased typeAliased
  where
    ensureNoDuplicates' ::
      (Ord op) =>
      (op -> SimpleErrorMessage) ->
      [FixityRecord op alias] ->
      m ()
    ensureNoDuplicates' toError =
      ensureNoDuplicates toError . map (\(i, pos, _, _) -> (i, pos))

    customOperatorTable' ::
      [FixityRecord op alias] ->
      [[(Qualified op, Associativity)]]
    customOperatorTable' = customOperatorTable . map (\(i, _, f, _) -> (i, f))

    makeLookupEntry :: FixityRecord op alias -> (Qualified op, Qualified alias)
    makeLookupEntry (qname, _, _, alias) = (qname, alias)

    renameAliasedOperators ::
      M.Map (Qualified (OpName 'ValueOpName)) (Qualified (Either Ident (ProperName 'ConstructorName))) ->
      M.Map (Qualified (OpName 'TypeOpName)) (Qualified (ProperName 'TypeName)) ->
      Module ->
      m Module
    renameAliasedOperators valueAliased typeAliased (Module ss coms mn ds exts) =
      Module ss coms mn <$> mapM (usingPredicate pred_ f') ds <*> pure exts
      where
        (goDecl', goExpr', goBinder') = updateTypes goType
        (f', _, _, _, _, _) =
          everywhereWithContextOnValuesM
            ss
            (\_ d -> (declSourceSpan d,) <$> goDecl' d)
            (\pos -> uncurry goExpr <=< goExpr' pos)
            (\pos -> uncurry goBinder <=< goBinder' pos)
            defS
            defS
            defS

        goExpr :: SourceSpan -> Expr -> m (SourceSpan, Expr)
        goExpr _ e@(PositionedValue pos _ _) = return (pos, e)
        goExpr _ (Op pos op) =
          (pos,) <$> case op `M.lookup` valueAliased of
            Just (Qualified mn' (Left alias)) ->
              return $ Var pos (Qualified mn' alias)
            Just (Qualified mn' (Right alias)) ->
              return $ Constructor pos (Qualified mn' alias)
            Nothing ->
              throwError . errorMessage' pos . UnknownName $ fmap ValOpName op
        goExpr pos other = return (pos, other)

        goBinder :: SourceSpan -> Binder -> m (SourceSpan, Binder)
        goBinder _ b@(PositionedBinder pos _ _) = return (pos, b)
        goBinder _ (BinaryNoParensBinder (OpBinder pos op) lhs rhs) =
          case op `M.lookup` valueAliased of
            Just (Qualified mn' (Left alias)) ->
              throwError . errorMessage' pos $ InvalidOperatorInBinder op (Qualified mn' alias)
            Just (Qualified mn' (Right alias)) ->
              return (pos, ConstructorBinder pos (Qualified mn' alias) [lhs, rhs])
            Nothing ->
              throwError . errorMessage' pos . UnknownName $ fmap ValOpName op
        goBinder _ BinaryNoParensBinder {} =
          internalError "BinaryNoParensBinder has no OpBinder"
        goBinder pos other = return (pos, other)

        goType :: SourceSpan -> SourceType -> m SourceType
        goType pos (TypeOp ann2 op) =
          case op `M.lookup` typeAliased of
            Just alias ->
              return $ TypeConstructor ann2 alias
            Nothing ->
              throwError . errorMessage' pos $ UnknownName $ fmap TyOpName op
        goType _ other = return other

-- | Indicates whether the `rebracketModule`
-- is being called with the full desugar pass
-- run via `purs compile` or whether
-- only the partial desugar pass is run
-- via `purs docs`.
-- This indication is needed to prevent
-- a `purs docs` error when using
-- `case _ of` syntax in a type class instance.
data RebracketCaller
  = CalledByCompile
  | CalledByDocs
  deriving (Eq, Show)

rebracketModule ::
  forall m.
  (MonadError MultipleErrors m) =>
  (MonadSupply m) =>
  RebracketCaller ->
  (Declaration -> Bool) ->
  [[(Qualified (OpName 'ValueOpName), Associativity)]] ->
  [[(Qualified (OpName 'TypeOpName), Associativity)]] ->
  Module ->
  m Module
rebracketModule !caller pred_ valueOpTable typeOpTable (Module ss coms mn ds exts) =
  Module ss coms mn <$> f' ds <*> pure exts
  where
    f' :: [Declaration] -> m [Declaration]
    f' =
      fmap (map (\d -> if pred_ d then removeParens d else d))
        . flip parU (usingPredicate pred_ h)

    -- The AST will run through all the desugar passes when compiling
    -- and only some of the desugar passes when generating docs.
    -- When generating docs, `case _ of` syntax used in an instance declaration
    -- can trigger the `IncorrectAnonymousArgument` error because it does not
    -- run the same passes that the compile desugaring does. Since `purs docs`
    -- will only succeed once `purs compile` succeeds, we can ignore this check
    -- when running `purs docs`.
    -- See https://github.com/purescript/purescript/issues/4274#issuecomment-1087730651=
    -- for more info.
    h :: Declaration -> m Declaration
    h = case caller of
      CalledByDocs -> f
      CalledByCompile -> g <=< f

    (f, _, _, _, _, _) =
      everywhereWithContextOnValuesM
        ss
        (\_ d -> (declSourceSpan d,) <$> goDecl d)
        (\pos -> wrap (matchExprOperators valueOpTable) <=< goExpr' pos)
        (\pos -> wrap (matchBinderOperators valueOpTable) <=< goBinder' pos)
        defS
        defS
        defS

    (g, _, _) = everywhereOnValuesTopDownM pure removeBinaryNoParens pure

    (goDecl, goExpr', goBinder') = updateTypes goType

    goType :: SourceSpan -> SourceType -> m SourceType
    goType = flip matchTypeOperators typeOpTable

    wrap :: (a -> m a) -> (SourceSpan, a) -> m (SourceSpan, a)
    wrap go (ss', a) = (ss',) <$> go a

removeBinaryNoParens :: (MonadError MultipleErrors m, MonadSupply m) => Expr -> m Expr
removeBinaryNoParens u
  | isAnonymousArgument u = case u of
      PositionedValue p _ _ -> rethrowWithPosition p err
      _ -> err
  where
    err = throwError . errorMessage $ IncorrectAnonymousArgument
removeBinaryNoParens (Parens (stripPositionInfo -> BinaryNoParens op l r))
  | isAnonymousArgument r = do
      arg <- freshIdent'
      return $ Abs (VarBinder nullSourceSpan arg) $ App (App op l) (Var nullSourceSpan (Qualified ByNullSourcePos arg))
  | isAnonymousArgument l = do
      arg <- freshIdent'
      return $ Abs (VarBinder nullSourceSpan arg) $ App (App op (Var nullSourceSpan (Qualified ByNullSourcePos arg))) r
removeBinaryNoParens (BinaryNoParens op l r) = return $ App (App op l) r
removeBinaryNoParens e = return e

stripPositionInfo :: Expr -> Expr
stripPositionInfo (PositionedValue _ _ e) = stripPositionInfo e
stripPositionInfo e = e

removeParens :: Declaration -> Declaration
removeParens = f
  where
    (f, _, _) =
      everywhereOnValues
        (runIdentity . goDecl)
        (goExpr . decontextify goExpr')
        (goBinder . decontextify goBinder')

    (goDecl, goExpr', goBinder') = updateTypes (\_ -> return . goType)

    goExpr :: Expr -> Expr
    goExpr (Parens val) = goExpr val
    goExpr val = val

    goBinder :: Binder -> Binder
    goBinder (ParensInBinder b) = goBinder b
    goBinder b = b

    goType :: Type a -> Type a
    goType (ParensInType _ t) = goType t
    goType t = t

    decontextify ::
      (SourceSpan -> a -> Identity (SourceSpan, a)) ->
      a ->
      a
    decontextify ctxf = snd . runIdentity . ctxf (internalError "attempted to use SourceSpan in removeParens")

externsFixities :: ExternsFile -> [Either ValueFixityRecord TypeFixityRecord]
externsFixities ExternsFile {..} =
  map (fromFixity efModuleName) efFixities ++ map (fromTypeFixity efModuleName) efTypeFixities


fromFixity ::
  P.ModuleName ->
  ExternsFixity ->
  Either ValueFixityRecord TypeFixityRecord
fromFixity mName (ExternsFixity assoc prec op name) =
  Left
    ( Qualified (ByModuleName mName) op,
      internalModuleSourceSpan "",
      Fixity assoc prec,
      name
    )

fromTypeFixity ::
  P.ModuleName ->
  ExternsTypeFixity ->
  Either ValueFixityRecord TypeFixityRecord
fromTypeFixity mName (ExternsTypeFixity assoc prec op name) =
  Right
    ( Qualified (ByModuleName mName) op,
      internalModuleSourceSpan "",
      Fixity assoc prec,
      name
    )

collectFixities :: Module -> [Either ValueFixityRecord TypeFixityRecord]
collectFixities (Module _ _ moduleName ds _) = concatMap collect ds
  where
    collect :: Declaration -> [Either ValueFixityRecord TypeFixityRecord]
    collect (ValueFixityDeclaration (ss, _) fixity name op) =
      [Left (Qualified (ByModuleName moduleName) op, ss, fixity, name)]
    collect (TypeFixityDeclaration (ss, _) fixity name op) =
      [Right (Qualified (ByModuleName moduleName) op, ss, fixity, name)]
    collect _ = []

ensureNoDuplicates ::
  (Ord a, MonadError MultipleErrors m) =>
  (a -> SimpleErrorMessage) ->
  [(Qualified a, SourceSpan)] ->
  m ()
ensureNoDuplicates toError m = go $ sortOn fst m
  where
    go [] = return ()
    go [_] = return ()
    go ((x@(Qualified (ByModuleName mn) op), _) : (y, pos) : _)
      | x == y =
          rethrow (addHint (ErrorInModule mn)) $
            rethrowWithPosition pos $
              throwError . errorMessage $
                toError op
    go (_ : rest) = go rest

customOperatorTable ::
  [(Qualified op, Fixity)] ->
  [[(Qualified op, Associativity)]]
customOperatorTable fixities =
  let userOps = map (\(name, Fixity a p) -> (name, p, a)) fixities
      sorted = sortOn (Down . (\(_, p, _) -> p)) userOps
      groups = groupBy ((==) `on` (\(_, p, _) -> p)) sorted
   in map (map (\(name, _, a) -> (name, a))) groups

updateTypes ::
  forall m.
  (Monad m) =>
  (SourceSpan -> SourceType -> m SourceType) ->
  ( Declaration -> m Declaration,
    SourceSpan -> Expr -> m (SourceSpan, Expr),
    SourceSpan -> Binder -> m (SourceSpan, Binder)
  )
updateTypes goType = (goDecl, goExpr, goBinder)
  where
    goType' :: SourceSpan -> SourceType -> m SourceType
    goType' = everywhereOnTypesTopDownM . goType

    goDecl :: Declaration -> m Declaration
    goDecl (DataDeclaration sa@(ss, _) ddt name args dctors) =
      DataDeclaration sa ddt name
        <$> traverse (traverse (traverse (goType' ss))) args
        <*> traverse (traverseDataCtorFields (traverse (sndM (goType' ss)))) dctors
    goDecl (ExternDeclaration sa@(ss, _) name ty) =
      ExternDeclaration sa name <$> goType' ss ty
    goDecl (TypeClassDeclaration sa@(ss, _) name args implies deps decls) = do
      implies' <- traverse (overConstraintArgs (traverse (goType' ss))) implies
      args' <- traverse (traverse (traverse (goType' ss))) args
      return $ TypeClassDeclaration sa name args' implies' deps decls
    goDecl (TypeInstanceDeclaration sa@(ss, _) na ch idx name cs className tys impls) = do
      cs' <- traverse (overConstraintArgs (traverse (goType' ss))) cs
      tys' <- traverse (goType' ss) tys
      return $ TypeInstanceDeclaration sa na ch idx name cs' className tys' impls
    goDecl (TypeSynonymDeclaration sa@(ss, _) name args ty) =
      TypeSynonymDeclaration sa name
        <$> traverse (traverse (traverse (goType' ss))) args
        <*> goType' ss ty
    goDecl (TypeDeclaration (TypeDeclarationData sa@(ss, _) expr ty)) =
      TypeDeclaration . TypeDeclarationData sa expr <$> goType' ss ty
    goDecl (KindDeclaration sa@(ss, _) sigFor name ty) =
      KindDeclaration sa sigFor name <$> goType' ss ty
    goDecl (ExternDataDeclaration sa@(ss, _) name ty) =
      ExternDataDeclaration sa name <$> goType' ss ty
    goDecl other =
      return other

    goExpr :: SourceSpan -> Expr -> m (SourceSpan, Expr)
    goExpr _ e@(PositionedValue pos _ _) = return (pos, e)
    goExpr pos (TypeClassDictionary (Constraint ann name kinds tys info) dicts hints) = do
      kinds' <- traverse (goType' pos) kinds
      tys' <- traverse (goType' pos) tys
      return (pos, TypeClassDictionary (Constraint ann name kinds' tys' info) dicts hints)
    goExpr pos (DeferredDictionary cls tys) = do
      tys' <- traverse (goType' pos) tys
      return (pos, DeferredDictionary cls tys')
    goExpr pos (TypedValue check v ty) = do
      ty' <- goType' pos ty
      return (pos, TypedValue check v ty')
    goExpr pos (VisibleTypeApp v ty) = do
      ty' <- goType' pos ty
      return (pos, VisibleTypeApp v ty')
    goExpr pos other = return (pos, other)

    goBinder :: SourceSpan -> Binder -> m (SourceSpan, Binder)
    goBinder _ e@(PositionedBinder pos _ _) = return (pos, e)
    goBinder pos (TypedBinder ty b) = do
      ty' <- goType' pos ty
      return (pos, TypedBinder ty' b)
    goBinder pos other = return (pos, other)

-- |
-- Checks all the fixity exports within a module to ensure that members aliased
-- by the operators are also exported from the module.
--
-- This pass requires name desugaring and export elaboration to have run first.
checkFixityExports ::
  forall m.
  (MonadError MultipleErrors m) =>
  Module ->
  m Module
checkFixityExports (Module _ _ _ _ Nothing) =
  internalError "exports should have been elaborated before checkFixityExports"
checkFixityExports m@(Module ss _ mn ds (Just exps)) =
  rethrow (addHint (ErrorInModule mn)) $
    rethrowWithPosition ss (traverse_ checkRef exps)
      $> m
  where
    checkRef :: DeclarationRef -> m ()
    checkRef dr@(ValueOpRef ss' op) =
      for_ (getValueOpAlias op) $ \case
        Left ident ->
          unless (ValueRef ss' ident `elem` exps)
            . throwError
            . errorMessage' ss'
            $ TransitiveExportError dr [ValueRef ss' ident]
        Right ctor ->
          unless (anyTypeRef (maybe False (elem ctor) . snd))
            . throwError
            . errorMessage' ss
            $ TransitiveDctorExportError dr [ctor]
    checkRef dr@(TypeOpRef ss' op) =
      for_ (getTypeOpAlias op) $ \ty ->
        unless (anyTypeRef ((== ty) . fst))
          . throwError
          . errorMessage' ss'
          $ TransitiveExportError dr [TypeRef ss' ty Nothing]
    checkRef _ = return ()

    -- Finds the name associated with a type operator when that type is also
    -- defined in the current module.
    getTypeOpAlias :: OpName 'TypeOpName -> Maybe (ProperName 'TypeName)
    getTypeOpAlias op =
      listToMaybe (mapMaybe (either (const Nothing) go <=< getFixityDecl) ds)
      where
        go (TypeFixity _ (Qualified (ByModuleName mn') ident) op')
          | mn == mn' && op == op' = Just ident
        go _ = Nothing

    -- Finds the value or data constructor associated with an operator when that
    -- declaration is also in the current module.
    getValueOpAlias ::
      OpName 'ValueOpName ->
      Maybe (Either Ident (ProperName 'ConstructorName))
    getValueOpAlias op =
      listToMaybe (mapMaybe (either go (const Nothing) <=< getFixityDecl) ds)
      where
        go (ValueFixity _ (Qualified (ByModuleName mn') ident) op')
          | mn == mn' && op == op' = Just ident
        go _ = Nothing

    -- Tests the exported `TypeRef` entries with a predicate.
    anyTypeRef ::
      ((ProperName 'TypeName, Maybe [ProperName 'ConstructorName]) -> Bool) ->
      Bool
    anyTypeRef f = any (maybe False f . getTypeRef) exps

usingPredicate ::
  forall f a.
  (Applicative f) =>
  (a -> Bool) ->
  (a -> f a) ->
  (a -> f a)
usingPredicate p f x =
  if p x then f x else pure x
