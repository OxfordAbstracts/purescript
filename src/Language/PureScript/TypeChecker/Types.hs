-- |
-- This module implements the type checker
--
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Language.PureScript.TypeChecker.Types
  ( BindingGroupType(..)
  , typesOf
  , checkTypeKind
  ) where

{-
  The following functions represent the corresponding type checking judgements:

    infer
      Synthesize a type for a value

    check
      Check a value has a given type

    checkProperties
      Check an object with a given type contains specified properties

    checkFunctionApplication
      Check a function of a given type returns a value of another type when applied to its arguments
-}

import Prelude
import Protolude (ordNub, fold, atMay)

import Control.Arrow (first, second, (***))
import Control.Monad (forM, forM_, guard, replicateM, unless, when, zipWithM, (<=<))
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.State.Class (MonadState(..), gets)
import Control.Monad.Supply.Class (MonadSupply)
import Control.Monad.Writer.Class (MonadWriter(..))

import Data.Bifunctor (bimap)
import Data.Either (partitionEithers)
import Data.Functor (($>))
import Data.List (transpose, (\\), partition, delete)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Traversable (for)
import Data.List.NonEmpty qualified as NEL
import Data.Map qualified as M
import Data.Set qualified as S
import Data.IntSet qualified as IS

import Language.PureScript.AST
import Language.PureScript.Crash (internalError)
import Language.PureScript.Environment
import Language.PureScript.Errors (ErrorMessage(..), MultipleErrors, SimpleErrorMessage(..), errorMessage, errorMessage', escalateWarningWhen, internalCompilerError, onErrorMessages, onTypesInErrorMessage, parU)
import Language.PureScript.Names (pattern ByNullSourcePos, Ident(..), ModuleName, Name(..), ProperName(..), ProperNameType(..), Qualified(..), QualifiedBy(..), byMaybeModuleName, coerceProperName, freshIdent)
import Language.PureScript.TypeChecker.Deriving (deriveInstance)
import Language.PureScript.TypeChecker.Entailment (InstanceContext, newDictionaries, replaceTypeClassDictionaries)
import Language.PureScript.TypeChecker.Kinds (checkConstraint, checkKind, checkTypeKind, kindOf, kindOfWithScopedVars, unifyKinds', unknownsWithKinds)
import Language.PureScript.TypeChecker.Monad
import Language.PureScript.TypeChecker.Skolems (introduceSkolemScope, newSkolemConstant, newSkolemScope, skolemEscapeCheck, skolemize, skolemizeTypesInValue)
import Language.PureScript.TypeChecker.Subsumption (subsumes)
import Language.PureScript.TypeChecker.Synonyms (replaceAllTypeSynonyms)
import Language.PureScript.TypeChecker.TypeSearch (typeSearch)
import Language.PureScript.TypeChecker.Unify (freshTypeWithKind, replaceTypeWildcards, substituteType, unifyTypes, unknownsInType, varIfUnknown)
import Language.PureScript.Types
import Language.PureScript.Label (Label(..))
import Language.PureScript.PSString (PSString)

data BindingGroupType
  = RecursiveBindingGroup
  | NonRecursiveBindingGroup
  deriving (Show, Eq, Ord)

-- | The result of a successful type check.
data TypedValue' = TypedValue' Bool Expr SourceType

-- | Convert an type checked value into an expression.
tvToExpr :: TypedValue' -> Expr
tvToExpr (TypedValue' c e t) = TypedValue c e t

-- | Lookup data about a type class in the @Environment@
lookupTypeClass :: MonadState CheckState TypeCheckM => Qualified (ProperName 'ClassName) -> TypeCheckM TypeClassData
lookupTypeClass name =
  let findClass = fromMaybe (internalError "entails: type class not found in environment") . M.lookup name
   in gets (findClass . typeClasses . checkEnv)

-- | Infer the types of multiple mutually-recursive values, and return elaborated values including
-- type class dictionaries and type annotations.
typesOf
  :: (MonadSupply TypeCheckM, MonadState CheckState TypeCheckM, MonadError MultipleErrors TypeCheckM, MonadWriter MultipleErrors TypeCheckM)
  => BindingGroupType
  -> ModuleName
  -> [((SourceAnn, Ident), Expr)]
  -> TypeCheckM [((SourceAnn, Ident), (Expr, SourceType))]
typesOf bindingGroupType moduleName vals = withFreshSubstitution $ do
    (tys, wInfer) <- capturingSubstitution tidyUp $ do
      (SplitBindingGroup untyped typed dict, w) <- withoutWarnings $ typeDictionaryForBindingGroup (Just moduleName) vals
      ds1 <- parU typed $ \e -> withoutWarnings $ checkTypedBindingGroupElement moduleName e dict
      ds2 <- forM untyped $ \e -> withoutWarnings $ typeForBindingGroupElement e dict
      return (map (False, ) ds1 ++ map (True, ) ds2, w)

    inferred <- forM tys $ \(shouldGeneralize, ((sai@((ss, _), ident), (val, ty)), _)) -> do
      -- Replace type class dictionary placeholders with actual dictionaries
      (val', unsolved) <- replaceTypeClassDictionaries shouldGeneralize val
      -- Generalize and constrain the type
      currentSubst <- gets checkSubstitution
      let ty' = substituteType currentSubst ty
          ty'' = constrain unsolved ty'
      unsolvedTypeVarsWithKinds <- unknownsWithKinds . IS.toList . unknowns $ constrain unsolved ty''
      let unsolvedTypeVars = IS.toList $ unknowns ty'

      generalized <- varIfUnknown unsolvedTypeVarsWithKinds ty''

      when shouldGeneralize $ do
        -- Show the inferred type in a warning
        tell
          . errorMessage' ss
          $ MissingTypeDeclaration ident generalized
        -- For non-recursive binding groups, can generalize over constraints.
        -- For recursive binding groups, we throw an error here for now.
        when (bindingGroupType == RecursiveBindingGroup && not (null unsolved))
          . throwError
          . errorMessage' ss
          $ CannotGeneralizeRecursiveFunction ident generalized
        -- We need information about functional dependencies, since we allow
        -- ambiguous types to be inferred if they can be solved by some functional
        -- dependency.
        conData <- forM unsolved $ \(_, _, con) -> do
          TypeClassData{ typeClassDependencies } <- lookupTypeClass $ constraintClass con
          let
            -- The set of unknowns mentioned in each argument.
            unknownsForArg :: [S.Set Int]
            unknownsForArg =
              map (S.fromList . map snd . unknownsInType) (constraintArgs con)
          pure (typeClassDependencies, unknownsForArg)
        -- Make sure any unsolved type constraints are determined by the
        -- type variables which appear unknown in the inferred type.
        let
          -- Take the closure of fundeps across constraints, to get more
          -- and more solved variables until reaching a fixpoint.
          solveFrom :: S.Set Int -> S.Set Int
          solveFrom determined = do
            let solved = solve1 determined
            if solved `S.isSubsetOf` determined
              then determined
              else solveFrom (determined <> solved)
          solve1 :: S.Set Int -> S.Set Int
          solve1 determined = fold $ do
            (tcDeps, conArgUnknowns) <- conData
            let
              lookupUnknowns :: Int -> Maybe (S.Set Int)
              lookupUnknowns = atMay conArgUnknowns
              unknownsDetermined :: Maybe (S.Set Int) -> Bool
              unknownsDetermined Nothing = False
              unknownsDetermined (Just unks) =
                unks `S.isSubsetOf` determined
            -- If all of the determining arguments of a particular fundep are
            -- already determined, add the determined arguments from the fundep
            tcDep <- tcDeps
            guard $ all (unknownsDetermined . lookupUnknowns) (fdDeterminers tcDep)
            map (fromMaybe S.empty . lookupUnknowns) (fdDetermined tcDep)
        -- These unknowns can be determined from the body of the inferred
        -- type (i.e. excluding the unknowns mentioned in the constraints)
        let determinedFromType = S.fromList unsolvedTypeVars
        -- These are all the unknowns mentioned in the constraints
        let constraintTypeVars = fold (conData >>= snd)
        let solved = solveFrom determinedFromType
        let unsolvedVars = S.difference constraintTypeVars solved
        let lookupUnkName' i = do
              mn <- lookupUnkName i
              pure (fromMaybe "t" mn, i)
        unsolvedVarNames <- traverse lookupUnkName' (S.toList unsolvedVars)
        unless (S.null unsolvedVars) .
          throwError
            . onErrorMessages (replaceTypes currentSubst)
            . errorMessage' ss
            $ AmbiguousTypeVariables generalized unsolvedVarNames

      -- Check skolem variables did not escape their scope
      skolemEscapeCheck val'
      return ((sai, (foldr (Abs . VarBinder nullSourceSpan . (\(x, _, _) -> x)) val' unsolved, generalized)), unsolved)

    -- Show warnings here, since types in wildcards might have been solved during
    -- instance resolution (by functional dependencies).
    finalState <- get
    let replaceTypes' = replaceTypes (checkSubstitution finalState)
        runTypeSearch' gen = runTypeSearch (guard gen $> foldMap snd inferred) finalState
        raisePreviousWarnings gen = escalateWarningWhen isHoleError . tell . onErrorMessages (runTypeSearch' gen . replaceTypes')

    raisePreviousWarnings False wInfer
    forM_ tys $ \(shouldGeneralize, ((_, (_, _)), w)) ->
      raisePreviousWarnings shouldGeneralize w

    return (map fst inferred)
  where
    replaceTypes
      :: Substitution
      -> ErrorMessage
      -> ErrorMessage
    replaceTypes subst = onTypesInErrorMessage (substituteType subst)

    -- Run type search to complete any typed hole error messages
    runTypeSearch
      :: Maybe [(Ident, InstanceContext, SourceConstraint)]
           -- Any unsolved constraints which we need to continue to satisfy
      -> CheckState
           -- The final type checker state
      -> ErrorMessage
      -> ErrorMessage
    runTypeSearch cons st = \case
      ErrorMessage hints (HoleInferredType x ty y (Just (TSBefore env))) ->
        let subst = checkSubstitution st
            searchResult = onTypeSearchTypes
              (substituteType subst)
              (uncurry TSAfter (typeSearch cons env st (substituteType subst ty)))
        in ErrorMessage hints (HoleInferredType x ty y (Just searchResult))
      other -> other

    -- Add any unsolved constraints
    constrain cs ty = foldr srcConstrainedType ty (map (\(_, _, x) -> x) cs)

    -- Apply the substitution that was returned from runUnify to both types and (type-annotated) values

    tidyUp ts sub = first (map (second (first (second (overTypes (substituteType sub) *** substituteType sub))))) ts

    isHoleError :: ErrorMessage -> Bool
    isHoleError (ErrorMessage _ HoleInferredType{}) = True
    isHoleError _ = False

-- | A binding group contains multiple value definitions, some of which are typed
-- and some which are not.
--
-- This structure breaks down a binding group into typed and untyped parts.
data SplitBindingGroup = SplitBindingGroup
  { _splitBindingGroupUntyped :: [((SourceAnn, Ident), (Expr, SourceType))]
  -- ^ The untyped expressions
  , _splitBindingGroupTyped :: [((SourceAnn, Ident), (Expr, [(Text, SourceType)], SourceType, Bool))]
  -- ^ The typed expressions, along with their type annotations
  , _splitBindingGroupNames :: M.Map (Qualified Ident) (SourceType, NameKind, NameVisibility)
  -- ^ A map containing all expressions and their assigned types (which might be
  -- fresh unification variables). These will be added to the 'Environment' after
  -- the binding group is checked, so the value type of the 'Map' is chosen to be
  -- compatible with the type of 'bindNames'.
  }

-- | This function breaks a binding group down into two sets of declarations:
-- those which contain type annotations, and those which don't.
-- This function also generates fresh unification variables for the types of
-- declarations without type annotations, returned in the 'UntypedData' structure.
typeDictionaryForBindingGroup
  :: (MonadState CheckState TypeCheckM, MonadError MultipleErrors TypeCheckM, MonadWriter MultipleErrors TypeCheckM)
  => Maybe ModuleName
  -> [((SourceAnn, Ident), Expr)]
  -> TypeCheckM SplitBindingGroup
typeDictionaryForBindingGroup moduleName vals = do
    -- Filter the typed and untyped declarations and make a map of names to typed declarations.
    -- Replace type wildcards here so that the resulting dictionary of types contains the
    -- fully expanded types.
    let (untyped, typed) = partitionEithers (map splitTypeAnnotation vals)
    (typedDict, typed') <- fmap unzip . for typed $ \(sai, (expr, ty, checkType)) -> do
      ((args, elabTy), kind) <- kindOfWithScopedVars ty
      checkTypeKind ty kind
      elabTy' <- replaceTypeWildcards elabTy
      return ((sai, elabTy'), (sai, (expr, args, elabTy', checkType)))
    -- Create fresh unification variables for the types of untyped declarations
    (untypedDict, untyped') <- fmap unzip . for untyped $ \(sai, expr) -> do
      ty <- freshTypeWithKind kindType
      return ((sai, ty), (sai, (expr, ty)))
    -- Create the dictionary of all name/type pairs, which will be added to the
    -- environment during type checking
    let dict = M.fromList [ (Qualified (maybe (BySourcePos $ spanStart ss) ByModuleName moduleName) ident, (ty, Private, Undefined))
                          | (((ss, _), ident), ty) <- typedDict <> untypedDict
                          ]
    return (SplitBindingGroup untyped' typed' dict)
  where
    -- Check if a value contains a type annotation, and if so, separate it
    -- from the value itself.
    splitTypeAnnotation :: (a, Expr) -> Either (a, Expr) (a, (Expr, SourceType, Bool))
    splitTypeAnnotation (a, TypedValue checkType value ty) = Right (a, (value, ty, checkType))
    splitTypeAnnotation (a, PositionedValue pos c value) =
      bimap (second (PositionedValue pos c))
            (second (\(e, t, b) -> (PositionedValue pos c e, t, b)))
            (splitTypeAnnotation (a, value))
    splitTypeAnnotation (a, value) = Left (a, value)

-- | Check the type annotation of a typed value in a binding group.
checkTypedBindingGroupElement
  :: (MonadSupply TypeCheckM, MonadState CheckState TypeCheckM, MonadError MultipleErrors TypeCheckM, MonadWriter MultipleErrors TypeCheckM)
  => ModuleName
  -> ((SourceAnn, Ident), (Expr, [(Text, SourceType)], SourceType, Bool))
  -- ^ The identifier we are trying to define, along with the expression and its type annotation
  -> M.Map (Qualified Ident) (SourceType, NameKind, NameVisibility)
  -- ^ Names brought into scope in this binding group
  -> TypeCheckM ((SourceAnn, Ident), (Expr, SourceType))
checkTypedBindingGroupElement mn (ident, (val, args, ty, checkType)) dict = do
  -- We replace type synonyms _after_ kind-checking, since we don't want type
  -- synonym expansion to bring type variables into scope. See #2542.
  ty' <- introduceSkolemScope <=< replaceAllTypeSynonyms $ ty
  -- Check the type with the new names in scope
  val' <- if checkType
            then withScopedTypeVars mn args $ bindNames dict $ check val ty'
            else return (TypedValue' False val ty')
  return (ident, (tvToExpr val', ty'))

-- | Infer a type for a value in a binding group which lacks an annotation.
typeForBindingGroupElement
  :: (MonadSupply TypeCheckM, MonadState CheckState TypeCheckM, MonadError MultipleErrors TypeCheckM, MonadWriter MultipleErrors TypeCheckM)
  => ((SourceAnn, Ident), (Expr, SourceType))
  -- ^ The identifier we are trying to define, along with the expression and its assigned type
  -- (at this point, this should be a unification variable)
  -> M.Map (Qualified Ident) (SourceType, NameKind, NameVisibility)
  -- ^ Names brought into scope in this binding group
  -> TypeCheckM ((SourceAnn, Ident), (Expr, SourceType))
typeForBindingGroupElement (ident, (val, ty)) dict = do
  -- Infer the type with the new names in scope
  TypedValue' _ val' ty' <- bindNames dict $ infer val
  -- Unify the type with the unification variable we chose for this definition
  unifyTypes ty ty'
  return (ident, (TypedValue True val' ty', ty'))

-- | Remove any ForAlls and ConstrainedType constructors in a type by introducing new unknowns
-- or TypeClassDictionary values.
--
-- This is necessary during type checking to avoid unifying a polymorphic type with a
-- unification variable.
instantiatePolyTypeWithUnknowns
  :: (MonadState CheckState TypeCheckM, MonadError MultipleErrors TypeCheckM)
  => Expr
  -> SourceType
  -> TypeCheckM (Expr, SourceType)
instantiatePolyTypeWithUnknowns val (ForAll _ _ ident mbK ty _) = do
  u <- maybe (internalCompilerError "Unelaborated forall") freshTypeWithKind mbK
  insertUnkName' u ident
  instantiatePolyTypeWithUnknowns val $ replaceTypeVars ident u ty
instantiatePolyTypeWithUnknowns val (ConstrainedType _ con ty) = do
  dicts <- getTypeClassDictionaries
  hints <- getHints
  instantiatePolyTypeWithUnknowns (App val (TypeClassDictionary con dicts hints)) ty
instantiatePolyTypeWithUnknowns val ty = return (val, ty)

instantiatePolyTypeWithUnknownsUntilVisible
  :: (MonadState CheckState TypeCheckM, MonadError MultipleErrors TypeCheckM)
  => Expr
  -> SourceType
  -> TypeCheckM (Expr, SourceType)
instantiatePolyTypeWithUnknownsUntilVisible val (ForAll _ TypeVarInvisible ident mbK ty _) = do
  u <- maybe (internalCompilerError "Unelaborated forall") freshTypeWithKind mbK
  insertUnkName' u ident
  instantiatePolyTypeWithUnknownsUntilVisible val $ replaceTypeVars ident u ty
instantiatePolyTypeWithUnknownsUntilVisible val ty = return (val, ty)

instantiateConstraint :: MonadState CheckState TypeCheckM => Expr -> Type SourceAnn -> TypeCheckM (Expr, Type SourceAnn)
instantiateConstraint val (ConstrainedType _ con ty) = do
  dicts <- getTypeClassDictionaries
  hints <- getHints
  instantiateConstraint (App val (TypeClassDictionary con dicts hints)) ty
instantiateConstraint val ty = pure (val, ty)

-- | Match against TUnknown and call insertUnkName, failing otherwise.
insertUnkName' :: (MonadState CheckState TypeCheckM, MonadError MultipleErrors TypeCheckM) => SourceType -> Text -> TypeCheckM ()
insertUnkName' (TUnknown _ i) n = insertUnkName i n
insertUnkName' _ _ = internalCompilerError "type is not TUnknown"

-- | Infer a type for a value, rethrowing any error to provide a more useful error message
infer
  :: (MonadSupply TypeCheckM, MonadState CheckState TypeCheckM, MonadError MultipleErrors TypeCheckM, MonadWriter MultipleErrors TypeCheckM)
  => Expr
  -> TypeCheckM TypedValue'
infer val = withErrorMessageHint (ErrorInferringType val) $ infer' val

-- | Infer a type for a value
infer'
  :: Expr
  -> TypeCheckM TypedValue'
infer' v@(Literal _ (NumericLiteral (Left _))) = return $ TypedValue' True v tyInt
infer' v@(Literal _ (NumericLiteral (Right _))) = return $ TypedValue' True v tyNumber
infer' v@(Literal _ (StringLiteral _)) = return $ TypedValue' True v tyString
infer' v@(Literal _ (CharLiteral _)) = return $ TypedValue' True v tyChar
infer' v@(Literal _ (BooleanLiteral _)) = return $ TypedValue' True v tyBoolean
infer' (Literal ss (ArrayLiteral vals)) = do
  ts <- traverse infer vals
  els <- freshTypeWithKind kindType
  ts' <- forM ts $ \(TypedValue' ch val t) -> do
    (val', t') <- instantiatePolyTypeWithUnknowns val t
    unifyTypes els t'
    return (TypedValue ch val' t')
  return $ TypedValue' True (Literal ss (ArrayLiteral ts')) (srcTypeApp tyArray els)
infer' (Literal ss (ObjectLiteral ps)) = do
  ensureNoDuplicateProperties ps
  typedFields <- inferProperties ps
  let
    toRowListItem :: (PSString, (Expr, SourceType)) -> RowListItem SourceAnn
    toRowListItem (l, (_, t)) = srcRowListItem (Label l) t

    recordType :: SourceType
    recordType = srcTypeApp tyRecord $ rowFromList (toRowListItem <$> typedFields, srcKindApp srcREmpty kindType)

    typedProperties :: [(PSString, Expr)]
    typedProperties = fmap (fmap (uncurry (TypedValue True))) typedFields
  pure $ TypedValue' True (Literal ss (ObjectLiteral typedProperties)) recordType
infer' (ObjectUpdate ob ps) = do
  ensureNoDuplicateProperties ps
  -- This "tail" holds all other fields not being updated.
  rowType <- freshTypeWithKind (kindRow kindType)
  let updateLabels = Label . fst <$> ps
  -- Generate unification variables for each field in ps.
  --
  -- Given:
  --
  -- ob { a = 0, b = 0 }
  --
  -- Then:
  --
  -- obTypes = [(a, ?0), (b, ?1)]
  obTypes <- zip updateLabels <$> replicateM (length updateLabels) (freshTypeWithKind kindType)
  let obItems :: [RowListItem SourceAnn]
      obItems = uncurry srcRowListItem <$> obTypes
      -- Create a record type that contains the unification variables.
      --
      -- obRecordType = Record ( a :: ?0, b :: ?1 | rowType )
      obRecordType :: SourceType
      obRecordType = srcTypeApp tyRecord $ rowFromList (obItems, rowType)
  -- Check ob against obRecordType.
  --
  -- Given:
  --
  -- ob : { a :: Int, b :: Int }
  --
  -- Then:
  --
  -- ?0  ~  Int
  -- ?1  ~  Int
  -- ob' : { a :: ?0, b :: ?1 }
  ob' <- TypedValue True <$> (tvToExpr <$> check ob obRecordType) <*> pure obRecordType
  -- Infer the types of the values used for the record update.
  typedFields <- inferProperties ps
  let newItems :: [RowListItem SourceAnn]
      newItems = (\(l, (_, t)) -> srcRowListItem (Label l) t) <$> typedFields

      ps' :: [(PSString, Expr)]
      ps' = (\(l, (e, t)) -> (l, TypedValue True e t)) <$> typedFields

      newRecordType :: SourceType
      newRecordType = srcTypeApp tyRecord $ rowFromList (newItems, rowType)
  pure $ TypedValue' True (ObjectUpdate ob' ps') newRecordType
infer' (Accessor prop val) = withErrorMessageHint (ErrorCheckingAccessor val prop) $ do
  field <- freshTypeWithKind kindType
  rest <- freshTypeWithKind (kindRow kindType)
  typed <- tvToExpr <$> check val (srcTypeApp tyRecord (srcRCons (Label prop) field rest))
  return $ TypedValue' True (Accessor prop typed) field
infer' (Abs binder ret)
  | VarBinder ss arg <- binder = do
      ty <- freshTypeWithKind kindType
      withBindingGroupVisible $ bindLocalVariables [(ss, arg, ty, Defined)] $ do
        body@(TypedValue' _ _ bodyTy) <- infer' ret
        (body', bodyTy') <- instantiatePolyTypeWithUnknowns (tvToExpr body) bodyTy
        return $ TypedValue' True (Abs (VarBinder ss arg) body') (function ty bodyTy')
  | otherwise = internalError "Binder was not desugared"
infer' (App f arg) = do
  f'@(TypedValue' _ _ ft) <- infer f
  (ret, app) <- checkFunctionApplication (tvToExpr f') ft arg
  return $ TypedValue' True app ret
infer' (VisibleTypeApp valFn (TypeWildcard _ _)) = do
  TypedValue' _ valFn' valTy <- infer valFn
  (valFn'', valTy') <- instantiatePolyTypeWithUnknownsUntilVisible valFn' valTy
  case valTy' of
    ForAll qAnn _ qName qKind qBody qSko -> do
      pure $ TypedValue' True valFn'' (ForAll qAnn TypeVarInvisible qName qKind qBody qSko)
    _ ->
      throwError $ errorMessage $ CannotSkipTypeApplication valTy'
infer' (VisibleTypeApp valFn tyArg) = do
  TypedValue' _ valFn' valTy <- infer valFn
  tyArg' <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards $ tyArg
  (valFn'', valTy') <- instantiatePolyTypeWithUnknownsUntilVisible valFn' valTy
  case valTy' of
    ForAll _ _ qName (Just qKind) qBody _ -> do
      tyArg'' <- replaceAllTypeSynonyms <=< checkKind tyArg' $ qKind
      let resTy = replaceTypeVars qName tyArg'' qBody
      (valFn''', resTy') <- instantiateConstraint valFn'' resTy
      pure $ TypedValue' True valFn''' resTy'
    _ ->
      throwError $ errorMessage $ CannotApplyExpressionOfTypeOnType valTy tyArg
infer' (Var ss var) = do
  checkVisibility var
  ty <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards <=< lookupVariable $ var
  case ty of
    ConstrainedType _ con ty' -> do
      dicts <- getTypeClassDictionaries
      hints <- getHints
      return $ TypedValue' True (App (Var ss var) (TypeClassDictionary con dicts hints)) ty'
    _ -> return $ TypedValue' True (Var ss var) ty
infer' v@(Constructor _ c) = do
  env <- getEnv
  case M.lookup c (dataConstructors env) of
    Nothing -> throwError . errorMessage . UnknownName . fmap DctorName $ c
    Just (_, _, ty, _) -> TypedValue' True v <$> (introduceSkolemScope <=< replaceAllTypeSynonyms $ ty)
infer' (Case vals binders) = do
  (vals', ts) <- instantiateForBinders vals binders
  ret <- freshTypeWithKind kindType
  binders' <- checkBinders ts ret binders
  return $ TypedValue' True (Case vals' binders') ret
infer' (IfThenElse cond th el) = do
  cond' <- tvToExpr <$> check cond tyBoolean
  th'@(TypedValue' _ _ thTy) <- infer th
  el'@(TypedValue' _ _ elTy) <- infer el
  (th'', thTy') <- instantiatePolyTypeWithUnknowns (tvToExpr th') thTy
  (el'', elTy') <- instantiatePolyTypeWithUnknowns (tvToExpr el') elTy
  unifyTypes thTy' elTy'
  return $ TypedValue' True (IfThenElse cond' th'' el'') thTy'
infer' (Let w ds val) = do
  (ds', tv@(TypedValue' _ _ valTy)) <- inferLetBinding [] ds val infer
  return $ TypedValue' True (Let w ds' (tvToExpr tv)) valTy
infer' (DeferredDictionary className tys) = do
  dicts <- getTypeClassDictionaries
  hints <- getHints
  con <- checkConstraint (srcConstraint className [] tys Nothing)
  return $ TypedValue' False
             (TypeClassDictionary con dicts hints)
             (foldl srcTypeApp (srcTypeConstructor (fmap coerceProperName className)) tys)
infer' (TypedValue checkType val ty) = do
  moduleName <- unsafeCheckCurrentModule
  ((args, elabTy), kind) <- kindOfWithScopedVars ty
  checkTypeKind ty kind
  ty' <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards $ elabTy
  tv <- if checkType then withScopedTypeVars moduleName args (check val ty') else return (TypedValue' False val ty)
  return $ TypedValue' True (tvToExpr tv) ty'
infer' (Hole name) = do
  ty <- freshTypeWithKind kindType
  ctx <- getLocalContext
  env <- getEnv
  tell . errorMessage $ HoleInferredType name ty ctx . Just $ TSBefore env
  return $ TypedValue' True (Hole name) ty
infer' (PositionedValue pos c val) = warnAndRethrowWithPositionTC pos $ do
  TypedValue' t v ty <- infer' val
  return $ TypedValue' t (PositionedValue pos c v) ty
infer' v = internalError $ "Invalid argument to infer: " ++ show v

-- |
-- Infer the types of named record fields.
inferProperties
  :: ( MonadSupply TypeCheckM
     , MonadState CheckState TypeCheckM
     , MonadError MultipleErrors TypeCheckM
     , MonadWriter MultipleErrors TypeCheckM
     )
  => [(PSString, Expr)]
  -> TypeCheckM [(PSString, (Expr, SourceType))]
inferProperties = traverse (traverse inferWithinRecord)

-- |
-- Infer the type of a value when used as a record field.
inferWithinRecord
  :: ( MonadSupply TypeCheckM
     , MonadState CheckState TypeCheckM
     , MonadError MultipleErrors TypeCheckM
     , MonadWriter MultipleErrors TypeCheckM
     )
  => Expr
  -> TypeCheckM (Expr, SourceType)
inferWithinRecord e = do
  TypedValue' _ v t <- infer e
  if propertyShouldInstantiate e
    then instantiatePolyTypeWithUnknowns v t
    else pure (v, t)

-- |
-- Determines if a value's type needs to be monomorphized when
-- used inside of a record.
propertyShouldInstantiate :: Expr -> Bool
propertyShouldInstantiate = \case
  Var{} -> True
  Constructor{} -> True
  VisibleTypeApp e _ -> propertyShouldInstantiate e
  PositionedValue _ _ e -> propertyShouldInstantiate e
  _ -> False

inferLetBinding
  :: (MonadSupply TypeCheckM, MonadState CheckState TypeCheckM, MonadError MultipleErrors TypeCheckM, MonadWriter MultipleErrors TypeCheckM)
  => [Declaration]
  -> [Declaration]
  -> Expr
  -> (Expr -> TypeCheckM TypedValue')
  -> TypeCheckM ([Declaration], TypedValue')
inferLetBinding seen [] ret j = (seen, ) <$> withBindingGroupVisible (j ret)
inferLetBinding seen (ValueDecl sa@(ss, _) ident nameKind [] [MkUnguarded (TypedValue checkType val ty)] : rest) ret j = do
  moduleName <- unsafeCheckCurrentModule
  TypedValue' _ val' ty'' <- warnAndRethrowWithPositionTC ss $ do
    ((args, elabTy), kind) <- kindOfWithScopedVars ty
    checkTypeKind ty kind
    let dict = M.singleton (Qualified (BySourcePos $ spanStart ss) ident) (elabTy, nameKind, Undefined)
    ty' <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards $ elabTy
    if checkType
      then withScopedTypeVars moduleName args (bindNames dict (check val ty'))
      else return (TypedValue' checkType val elabTy)
  bindNames (M.singleton (Qualified (BySourcePos $ spanStart ss) ident) (ty'', nameKind, Defined))
    $ inferLetBinding (seen ++ [ValueDecl sa ident nameKind [] [MkUnguarded (TypedValue checkType val' ty'')]]) rest ret j
inferLetBinding seen (ValueDecl sa@(ss, _) ident nameKind [] [MkUnguarded val] : rest) ret j = do
  valTy <- freshTypeWithKind kindType
  TypedValue' _ val' valTy' <- warnAndRethrowWithPositionTC ss $ do
    let dict = M.singleton (Qualified (BySourcePos $ spanStart ss) ident) (valTy, nameKind, Undefined)
    bindNames dict $ infer val
  warnAndRethrowWithPositionTC ss $ unifyTypes valTy valTy'
  bindNames (M.singleton (Qualified (BySourcePos $ spanStart ss) ident) (valTy', nameKind, Defined))
    $ inferLetBinding (seen ++ [ValueDecl sa ident nameKind [] [MkUnguarded val']]) rest ret j
inferLetBinding seen (BindingGroupDeclaration ds : rest) ret j = do
  moduleName <- unsafeCheckCurrentModule
  SplitBindingGroup untyped typed dict <- typeDictionaryForBindingGroup Nothing . NEL.toList $ fmap (\(i, _, v) -> (i, v)) ds
  ds1' <- parU typed $ \e -> checkTypedBindingGroupElement moduleName e dict
  ds2' <- forM untyped $ \e -> typeForBindingGroupElement e dict
  let ds' = NEL.fromList [(ident, Private, val') | (ident, (val', _)) <- ds1' ++ ds2']
  bindNames dict $ do
    makeBindingGroupVisible
    inferLetBinding (seen ++ [BindingGroupDeclaration ds']) rest ret j
inferLetBinding _ _ _ _ = internalError "Invalid argument to inferLetBinding"

-- | Infer the types of variables brought into scope by a binder
inferBinder
  :: SourceType
  -> Binder
  -> TypeCheckM (M.Map Ident (SourceSpan, SourceType))
inferBinder _ NullBinder = return M.empty
inferBinder val (LiteralBinder _ (StringLiteral _)) = unifyTypes val tyString >> return M.empty
inferBinder val (LiteralBinder _ (CharLiteral _)) = unifyTypes val tyChar >> return M.empty
inferBinder val (LiteralBinder _ (NumericLiteral (Left _))) = unifyTypes val tyInt >> return M.empty
inferBinder val (LiteralBinder _ (NumericLiteral (Right _))) = unifyTypes val tyNumber >> return M.empty
inferBinder val (LiteralBinder _ (BooleanLiteral _)) = unifyTypes val tyBoolean >> return M.empty
inferBinder val (VarBinder ss name) = return $ M.singleton name (ss, val)
inferBinder val (ConstructorBinder ss ctor binders) = do
  env <- getEnv
  case M.lookup ctor (dataConstructors env) of
    Just (_, _, ty, _) -> do
      (_, fn) <- instantiatePolyTypeWithUnknowns (internalError "Data constructor types cannot contain constraints") ty
      fn' <- introduceSkolemScope <=< replaceAllTypeSynonyms $ fn
      let (args, ret) = peelArgs fn'
          expected = length args
          actual = length binders
      unless (expected == actual) . throwError . errorMessage' ss $ IncorrectConstructorArity ctor expected actual
      unifyTypes ret val
      M.unions <$> zipWithM inferBinder (reverse args) binders
    _ -> throwError . errorMessage' ss . UnknownName . fmap DctorName $ ctor
  where
  peelArgs :: Type a -> ([Type a], Type a)
  peelArgs = go []
    where
    go args (TypeApp _ (TypeApp _ fn arg) ret) | eqType fn tyFunction = go (arg : args) ret
    go args ret = (args, ret)
inferBinder val (LiteralBinder _ (ObjectLiteral props)) = do
  row <- freshTypeWithKind (kindRow kindType)
  rest <- freshTypeWithKind (kindRow kindType)
  m1 <- inferRowProperties row rest props
  unifyTypes val (srcTypeApp tyRecord row)
  return m1
  where
  inferRowProperties :: SourceType -> SourceType -> [(PSString, Binder)] -> TypeCheckM (M.Map Ident (SourceSpan, SourceType))
  inferRowProperties nrow row [] = unifyTypes nrow row >> return M.empty
  inferRowProperties nrow row ((name, binder):binders) = do
    propTy <- freshTypeWithKind kindType
    m1 <- inferBinder propTy binder
    m2 <- inferRowProperties nrow (srcRCons (Label name) propTy row) binders
    return $ m1 `M.union` m2
inferBinder val (LiteralBinder _ (ArrayLiteral binders)) = do
  el <- freshTypeWithKind kindType
  m1 <- M.unions <$> traverse (inferBinder el) binders
  unifyTypes val (srcTypeApp tyArray el)
  return m1
inferBinder val (NamedBinder ss name binder) =
  warnAndRethrowWithPositionTC ss $ do
    m <- inferBinder val binder
    return $ M.insert name (ss, val) m
inferBinder val (PositionedBinder pos _ binder) =
  warnAndRethrowWithPositionTC pos $ inferBinder val binder
inferBinder val (TypedBinder ty binder) = do
  (elabTy, kind) <- kindOf ty
  checkTypeKind ty kind
  ty1 <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards $ elabTy
  unifyTypes val ty1
  inferBinder ty1 binder
inferBinder _ OpBinder{} =
  internalError "OpBinder should have been desugared before inferBinder"
inferBinder _ BinaryNoParensBinder{} =
  internalError "BinaryNoParensBinder should have been desugared before inferBinder"
inferBinder _ ParensInBinder{} =
  internalError "ParensInBinder should have been desugared before inferBinder"

-- | Returns true if a binder requires its argument type to be a monotype.
-- | If this is the case, we need to instantiate any polymorphic types before checking binders.
binderRequiresMonotype :: Binder -> Bool
binderRequiresMonotype NullBinder = False
binderRequiresMonotype (VarBinder _ _) = False
binderRequiresMonotype (NamedBinder _ _ b) = binderRequiresMonotype b
binderRequiresMonotype (PositionedBinder _ _ b) = binderRequiresMonotype b
binderRequiresMonotype (TypedBinder ty b) = isMonoType ty || binderRequiresMonotype b
binderRequiresMonotype _ = True

-- | Instantiate polytypes only when necessitated by a binder.
instantiateForBinders
  :: (MonadSupply TypeCheckM, MonadState CheckState TypeCheckM, MonadError MultipleErrors TypeCheckM, MonadWriter MultipleErrors TypeCheckM)
  => [Expr]
  -> [CaseAlternative]
  -> TypeCheckM ([Expr], [SourceType])
instantiateForBinders vals cas = unzip <$> zipWithM (\val inst -> do
  TypedValue' _ val' ty <- infer val
  if inst
    then instantiatePolyTypeWithUnknowns val' ty
    else return (val', ty)) vals shouldInstantiate
  where
  shouldInstantiate :: [Bool]
  shouldInstantiate = map (any binderRequiresMonotype) . transpose . map caseAlternativeBinders $ cas

-- |
-- Check the types of the return values in a set of binders in a case statement
--
checkBinders
  :: (MonadSupply TypeCheckM, MonadState CheckState TypeCheckM, MonadError MultipleErrors TypeCheckM, MonadWriter MultipleErrors TypeCheckM)
  => [SourceType]
  -> SourceType
  -> [CaseAlternative]
  -> TypeCheckM [CaseAlternative]
checkBinders _ _ [] = return []
checkBinders nvals ret (CaseAlternative binders result : bs) = do
  guardWith (errorMessage $ OverlappingArgNames Nothing) $
    let ns = concatMap binderNames binders in length (ordNub ns) == length ns
  m1 <- M.unions <$> zipWithM inferBinder nvals binders
  r <- bindLocalVariables [ (ss, name, ty, Defined) | (name, (ss, ty)) <- M.toList m1 ] $
       CaseAlternative binders <$> forM result (\ge -> checkGuardedRhs ge ret)
  rs <- checkBinders nvals ret bs
  return $ r : rs

checkGuardedRhs
  :: (MonadSupply TypeCheckM, MonadState CheckState TypeCheckM, MonadError MultipleErrors TypeCheckM, MonadWriter MultipleErrors TypeCheckM)
  => GuardedExpr
  -> SourceType
  -> TypeCheckM GuardedExpr
checkGuardedRhs (GuardedExpr [] rhs) ret = do
  rhs' <- TypedValue True <$> (tvToExpr <$> check rhs ret) <*> pure ret
  return $ GuardedExpr [] rhs'
checkGuardedRhs (GuardedExpr (ConditionGuard cond : guards) rhs) ret = do
  cond' <- withErrorMessageHint ErrorCheckingGuard $ check cond tyBoolean
  GuardedExpr guards' rhs' <- checkGuardedRhs (GuardedExpr guards rhs) ret
  return $ GuardedExpr (ConditionGuard (tvToExpr cond') : guards') rhs'
checkGuardedRhs (GuardedExpr (PatternGuard binder expr : guards) rhs) ret = do
  tv@(TypedValue' _ _ ty) <- infer expr
  variables <- inferBinder ty binder
  GuardedExpr guards' rhs' <- bindLocalVariables [ (ss, name, bty, Defined)
                                                 | (name, (ss, bty)) <- M.toList variables
                                                 ] $
    checkGuardedRhs (GuardedExpr guards rhs) ret
  return $ GuardedExpr (PatternGuard binder (tvToExpr tv) : guards') rhs'

-- |
-- Check the type of a value, rethrowing errors to provide a better error message
--
check
  :: (MonadSupply TypeCheckM, MonadState CheckState TypeCheckM, MonadError MultipleErrors TypeCheckM, MonadWriter MultipleErrors TypeCheckM)
  => Expr
  -> SourceType
  -> TypeCheckM TypedValue'
check val ty = withErrorMessageHint' val (ErrorCheckingType val ty) $ check' val ty

-- |
-- Check the type of a value
--
check'
  :: Expr
  -> SourceType
  -> TypeCheckM TypedValue'
check' val (ForAll ann vis ident mbK ty _) = do
  env <- getEnv
  mn <- gets checkCurrentModule
  scope <- newSkolemScope
  sko <- newSkolemConstant
  let ss = case val of
             PositionedValue pos c _ -> (pos, c)
             _ -> NullSourceAnn
      sk = skolemize ss ident mbK sko scope ty
      -- We should only skolemize types in values when the type variable
      -- was actually brought into scope. Otherwise we can end up skolemizing
      -- an undefined type variable that happens to clash with the variable we
      -- want to skolemize. This can happen due to synonym expansion (see 2542).
      skVal
        | Just _ <- M.lookup (Qualified (byMaybeModuleName mn) (ProperName ident)) $ types env =
            skolemizeTypesInValue ss ident mbK sko scope val
        | otherwise = val
  val' <- tvToExpr <$> check skVal sk
  return $ TypedValue' True val' (ForAll ann vis ident mbK ty (Just scope))
check' val t@(ConstrainedType _ con@(Constraint _ cls@(Qualified _ (ProperName className)) _ _ _) ty) = do
  TypeClassData{ typeClassIsEmpty } <- lookupTypeClass cls
  -- An empty class dictionary is never used; see code in `TypeChecker.Entailment`
  -- that wraps empty dictionary solutions in `Unused`.
  dictName <- if typeClassIsEmpty then pure UnusedIdent else freshIdent ("dict" <> className)
  dicts <- newDictionaries [] (Qualified ByNullSourcePos dictName) con
  val' <- withBindingGroupVisible $ withTypeClassDictionaries dicts $ check val ty
  return $ TypedValue' True (Abs (VarBinder nullSourceSpan dictName) (tvToExpr val')) t
check' val u@(TUnknown _ _) = do
  val'@(TypedValue' _ _ ty) <- infer val
  -- Don't unify an unknown with an inferred polytype
  (val'', ty') <- instantiatePolyTypeWithUnknowns (tvToExpr val') ty
  unifyTypes ty' u
  return $ TypedValue' True val'' ty'
check' v@(Literal _ (NumericLiteral (Left _))) t | t == tyInt =
  return $ TypedValue' True v t
check' v@(Literal _ (NumericLiteral (Right _))) t | t == tyNumber =
  return $ TypedValue' True v t
check' v@(Literal _ (StringLiteral _)) t | t == tyString =
  return $ TypedValue' True v t
check' v@(Literal _ (CharLiteral _)) t | t == tyChar =
  return $ TypedValue' True v t
check' v@(Literal _ (BooleanLiteral _)) t | t == tyBoolean =
  return $ TypedValue' True v t
check' (Literal ss (ArrayLiteral vals)) t@(TypeApp _ a ty) = do
  unifyTypes a tyArray
  array <- Literal ss . ArrayLiteral . map tvToExpr <$> forM vals (`check` ty)
  return $ TypedValue' True array t
check' (Abs binder ret) ty@(TypeApp _ (TypeApp _ t argTy) retTy)
  | VarBinder ss arg <- binder = do
      unifyTypes t tyFunction
      ret' <- withBindingGroupVisible $ bindLocalVariables [(ss, arg, argTy, Defined)] $ check ret retTy
      return $ TypedValue' True (Abs (VarBinder ss arg) (tvToExpr ret')) ty
  | otherwise = internalError "Binder was not desugared"
check' (App f arg) ret = do
  f'@(TypedValue' _ _ ft) <- infer f
  (retTy, app) <- checkFunctionApplication (tvToExpr f') ft arg
  elaborate <- subsumes retTy ret
  return $ TypedValue' True (elaborate app) ret
check' v@(Var _ var) ty = do
  checkVisibility var
  repl <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< lookupVariable $ var
  ty' <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards $ ty
  elaborate <- subsumes repl ty'
  return $ TypedValue' True (elaborate v) ty'
check' (DeferredDictionary className tys) ty = do
  {-
  -- Here, we replace a placeholder for a superclass dictionary with a regular
  -- TypeClassDictionary placeholder. The reason we do this is that it is necessary to have the
  -- correct super instance dictionaries in scope, and these are not available when the type class
  -- declaration gets desugared.
  -}
  dicts <- getTypeClassDictionaries
  hints <- getHints
  con <- checkConstraint (srcConstraint className [] tys Nothing)
  return $ TypedValue' False
             (TypeClassDictionary con dicts hints)
             ty
check' (TypedValue checkType val ty1) ty2 = do
  moduleName <- unsafeCheckCurrentModule
  ((args, elabTy1), kind1) <- kindOfWithScopedVars ty1
  (elabTy2, kind2) <- kindOf ty2
  unifyKinds' kind1 kind2
  checkTypeKind ty1 kind1
  ty1' <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards $ elabTy1
  ty2' <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards $ elabTy2
  elaborate <- subsumes ty1' ty2'
  val' <- if checkType
            then withScopedTypeVars moduleName args $ tvToExpr <$> check val ty1'
            else pure val
  return $ TypedValue' True (TypedValue checkType (elaborate val') ty1') ty2'
check' (Case vals binders) ret = do
  (vals', ts) <- instantiateForBinders vals binders
  binders' <- checkBinders ts ret binders
  return $ TypedValue' True (Case vals' binders') ret
check' (IfThenElse cond th el) ty = do
  cond' <- tvToExpr <$> check cond tyBoolean
  th' <- tvToExpr <$> check th ty
  el' <- tvToExpr <$> check el ty
  return $ TypedValue' True (IfThenElse cond' th' el') ty
check' e@(Literal ss (ObjectLiteral ps)) t@(TypeApp _ obj row) | obj == tyRecord = do
  ensureNoDuplicateProperties ps
  ps' <- checkProperties e ps row False
  return $ TypedValue' True (Literal ss (ObjectLiteral ps')) t
check' (DerivedInstancePlaceholder name strategy) t = do
  d <- deriveInstance t name strategy
  d' <- tvToExpr <$> check' d t
  return $ TypedValue' True d' t
check' e@(ObjectUpdate obj ps) t@(TypeApp _ o row) | o == tyRecord = do
  ensureNoDuplicateProperties ps
  -- We need to be careful to avoid duplicate labels here.
  -- We check _obj_ against the type _t_ with the types in _ps_ replaced with unknowns.
  let (propsToCheck, rest) = rowToList row
      (removedProps, remainingProps) = partition (\(RowListItem _ p _) -> p `elem` map (Label . fst) ps) propsToCheck
  us <- zipWith srcRowListItem (map rowListLabel removedProps) <$> replicateM (length ps) (freshTypeWithKind kindType)
  obj' <- tvToExpr <$> check obj (srcTypeApp tyRecord (rowFromList (us ++ remainingProps, rest)))
  ps' <- checkProperties e ps row True
  return $ TypedValue' True (ObjectUpdate obj' ps') t
check' (Accessor prop val) ty = withErrorMessageHint (ErrorCheckingAccessor val prop) $ do
  rest <- freshTypeWithKind (kindRow kindType)
  val' <- tvToExpr <$> check val (srcTypeApp tyRecord (srcRCons (Label prop) ty rest))
  return $ TypedValue' True (Accessor prop val') ty
check' v@(Constructor _ c) ty = do
  env <- getEnv
  case M.lookup c (dataConstructors env) of
    Nothing -> throwError . errorMessage . UnknownName . fmap DctorName $ c
    Just (_, _, ty1, _) -> do
      repl <- introduceSkolemScope <=< replaceAllTypeSynonyms $ ty1
      ty' <- introduceSkolemScope <=< replaceAllTypeSynonyms $ ty
      elaborate <- subsumes repl ty'
      return $ TypedValue' True (elaborate v) ty'
check' (Let w ds val) ty = do
  (ds', val') <- inferLetBinding [] ds val (`check` ty)
  return $ TypedValue' True (Let w ds' (tvToExpr val')) ty
check' val kt@(KindedType _ ty kind) = do
  checkTypeKind ty kind
  val' <- tvToExpr <$> check' val ty
  return $ TypedValue' True val' kt
check' (PositionedValue pos c val) ty = warnAndRethrowWithPositionTC pos $ do
  TypedValue' t v ty' <- check' val ty
  return $ TypedValue' t (PositionedValue pos c v) ty'
check' val ty = do
  TypedValue' _ val' ty' <- infer val
  elaborate <- subsumes ty' ty
  return $ TypedValue' True (elaborate val') ty

-- |
-- Check the type of a collection of named record fields
--
-- The @lax@ parameter controls whether or not every record member has to be provided. For object updates, this is not the case.
--
checkProperties
  :: (MonadSupply TypeCheckM, MonadState CheckState TypeCheckM, MonadError MultipleErrors TypeCheckM, MonadWriter MultipleErrors TypeCheckM)
  => Expr
  -> [(PSString, Expr)]
  -> SourceType
  -> Bool
  -> TypeCheckM [(PSString, Expr)]
checkProperties expr ps row lax = convert <$> go ps (toRowPair <$> ts') r' where
  convert = fmap (fmap tvToExpr)
  (ts', r') = rowToList row
  toRowPair (RowListItem _ lbl ty) = (lbl, ty)
  go [] [] (REmptyKinded _ _) = return []
  go [] [] u@(TUnknown _ _)
    | lax = return []
    | otherwise = do unifyTypes u srcREmpty
                     return []
  go [] [] Skolem{} | lax = return []
  go [] ((p, _): _) _ | lax = return []
                      | otherwise = throwError . errorMessage $ PropertyIsMissing p
  go ((p,_):_) [] (REmptyKinded _ _) = throwError . errorMessage $ AdditionalProperty $ Label p
  go ((p,v):ps') ts r =
    case lookup (Label p) ts of
      Nothing -> do
        (v', ty) <- inferWithinRecord v
        rest <- freshTypeWithKind (kindRow kindType)
        unifyTypes r (srcRCons (Label p) ty rest)
        ps'' <- go ps' ts rest
        return $ (p, TypedValue' True v' ty) : ps''
      Just ty -> do
        v' <- check v ty
        ps'' <- go ps' (delete (Label p, ty) ts) r
        return $ (p, v') : ps''
  go _ _ _ = throwError . errorMessage $ ExprDoesNotHaveType expr (srcTypeApp tyRecord row)

-- | Check the type of a function application, rethrowing errors to provide a better error message.
--
-- This judgment takes three inputs:
--
-- * The expression of the function we are applying
-- * The type of that function
-- * The expression we are applying it to
--
-- and synthesizes two outputs:
--
-- * The return type
-- * The elaborated expression for the function application (since we might need to
--   insert type class dictionaries, etc.)
checkFunctionApplication
  :: (MonadSupply TypeCheckM, MonadState CheckState TypeCheckM, MonadError MultipleErrors TypeCheckM, MonadWriter MultipleErrors TypeCheckM)
  => Expr
  -- ^ The function expression
  -> SourceType
  -- ^ The type of the function
  -> Expr
  -- ^ The argument expression
  -> TypeCheckM (SourceType, Expr)
  -- ^ The result type, and the elaborated term
checkFunctionApplication fn fnTy arg = withErrorMessageHint' fn (ErrorInApplication fn fnTy arg) $ do
  subst <- gets checkSubstitution
  checkFunctionApplication' fn (substituteType subst fnTy) arg

-- | Check the type of a function application
checkFunctionApplication'
  :: (MonadSupply TypeCheckM, MonadState CheckState TypeCheckM, MonadError MultipleErrors TypeCheckM, MonadWriter MultipleErrors TypeCheckM)
  => Expr
  -> SourceType
  -> Expr
  -> TypeCheckM (SourceType, Expr)
checkFunctionApplication' fn (TypeApp _ (TypeApp _ tyFunction' argTy) retTy) arg = do
  unifyTypes tyFunction' tyFunction
  arg' <- tvToExpr <$> check arg argTy
  return (retTy, App fn arg')
checkFunctionApplication' fn (ForAll _ _ ident mbK ty _) arg = do
  u <- maybe (internalCompilerError "Unelaborated forall") freshTypeWithKind mbK
  insertUnkName' u ident
  let replaced = replaceTypeVars ident u ty
  checkFunctionApplication fn replaced arg
checkFunctionApplication' fn (KindedType _ ty _) arg =
  checkFunctionApplication fn ty arg
checkFunctionApplication' fn (ConstrainedType _ con fnTy) arg = do
  dicts <- getTypeClassDictionaries
  hints <- getHints
  checkFunctionApplication' (App fn (TypeClassDictionary con dicts hints)) fnTy arg
checkFunctionApplication' fn fnTy dict@TypeClassDictionary{} =
  return (fnTy, App fn dict)
checkFunctionApplication' fn u arg = do
  tv@(TypedValue' _ _ ty) <- do
    TypedValue' _ arg' t <- infer arg
    (arg'', t') <- instantiatePolyTypeWithUnknowns arg' t
    return $ TypedValue' True arg'' t'
  ret <- freshTypeWithKind kindType
  unifyTypes u (function ty ret)
  return (ret, App fn (tvToExpr tv))

-- |
-- Ensure a set of property names and value does not contain duplicate labels
--
ensureNoDuplicateProperties :: (MonadError MultipleErrors TypeCheckM) => [(PSString, Expr)] -> TypeCheckM ()
ensureNoDuplicateProperties ps =
  let ls = map fst ps in
  case ls \\ ordNub ls of
    l : _ -> throwError . errorMessage $ DuplicateLabel (Label l) Nothing
    _ -> return ()

-- | Test if this is an internal value to be excluded from error hints
isInternal :: Expr -> Bool
isInternal = \case
  PositionedValue _ _ v -> isInternal v
  TypedValue _ v _ -> isInternal v
  Constructor _ (Qualified _ name) -> isDictTypeName name
  DerivedInstancePlaceholder{} -> True
  _ -> False

-- | Introduce a hint only if the given expression is not internal
withErrorMessageHint'
  :: (MonadState CheckState TypeCheckM, MonadError MultipleErrors TypeCheckM)
  => Expr
  -> ErrorMessageHint
  -> TypeCheckM a
  -> TypeCheckM a
withErrorMessageHint' expr = if isInternal expr then const id else withErrorMessageHint
