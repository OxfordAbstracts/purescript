{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-record-wildcards #-}

module Language.PureScript.Lsp.Handlers.Hover where

import Control.Arrow ((>>>))
import Control.Exception.Lifted (catch, handle)
import Control.Lens (Field1 (_1), Field2 (_2), Field3 (_3), (^.))
import Control.Lens.Combinators (view)
import Control.Monad.Supply (runSupplyT)
import Control.Monad.Trans.Writer (WriterT (runWriterT))
import Control.Monad.Writer (MonadWriter (..), censor)
import Data.List (last)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Text qualified as T
import GHC.TopHandler (runIO)
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as Message
import Language.LSP.Protocol.Types qualified as Types
import Language.LSP.Server qualified as Server
import Language.PureScript (evalSupplyT)
import Language.PureScript qualified as P
import Language.PureScript.AST.Binders (Binder (..))
import Language.PureScript.Docs.Convert.Single (convertComments)
import Language.PureScript.Docs.Types qualified as Docs
import Language.PureScript.Environment (tyBoolean, tyChar, tyInt, tyNumber, tyString)
import Language.PureScript.Errors (Literal (..))
import Language.PureScript.Ide.Error (prettyPrintTypeSingleLine)
import Language.PureScript.Lsp.AtPosition (EverythingAtPos (..), debugExpr, getChildExprs, getEverythingAtPos, getImportRefNameType, modifySmallestExprAtPos, showCounts, spanSize, spanToRange)
import Language.PureScript.Lsp.Cache (selectDependencies)
import Language.PureScript.Lsp.Cache.Query (getAstDeclarationTypeInModule)
import Language.PureScript.Lsp.Docs (readDeclarationDocsWithNameType, readModuleDocs)
import Language.PureScript.Lsp.Log (debugLsp)
import Language.PureScript.Lsp.Monad (HandlerM)
import Language.PureScript.Lsp.NameType (LspNameType (..))
import Language.PureScript.Lsp.Print (printName)
import Language.PureScript.Lsp.Rebuild (buildExportEnvCacheAndHandleErrors)
import Language.PureScript.Lsp.State (cachedRebuild, getExportEnv)
import Language.PureScript.Lsp.Types (ExternDependency (edExtern), OpenFile (..))
import Language.PureScript.Lsp.Util (declsAtLine, posInSpan, sourcePosToPosition)
import Language.PureScript.Names (disqualify)
import Language.PureScript.TypeChecker (getEnv)
import Language.PureScript.TypeChecker.Types (infer')
import Language.PureScript.TypeChecker.Unify (unifyTypes)
import Protolude hiding (handle, to)
import Text.PrettyPrint.Boxes (render)

hoverHandler :: Server.Handlers HandlerM
hoverHandler = Server.requestHandler Message.SMethod_TextDocumentHover $ \req res -> do
  let Types.HoverParams docIdent startPos _prog = req ^. LSP.params
      filePathMb = Types.uriToFilePath $ docIdent ^. LSP.uri

      nullRes = res $ Right $ Types.InR Types.Null

      markdownRes md range =
        res $ Right $ Types.InL $ Types.Hover (Types.InL $ Types.MarkupContent Types.MarkupKind_Markdown md) range

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

      respondWithSourceType :: P.SourceType -> HandlerM ()
      respondWithSourceType tipe = do
        let printedType = prettyPrintTypeSingleLine tipe
        markdownRes (pursTypeStr "_" (Just printedType) []) (Just $ spanToRange $ fst $ P.getAnnForType tipe)

      respondWithExprDebug :: Text -> P.SourceSpan -> P.Expr -> HandlerM ()
      respondWithExprDebug label ss expr = do
        let printedExpr = ellipsis 2000 $ show expr
        markdownRes (label <> ": \n" <> pursMd printedExpr) (Just $ spanToRange ss)

      respondWithExpr2Debug :: Text -> Text -> P.SourceSpan -> P.Expr -> P.Expr -> HandlerM ()
      respondWithExpr2Debug label label' ss expr expr' = do
        let printedExpr = ellipsis 2000 $ show expr
            printedExpr' = ellipsis 2000 $ show expr'
        markdownRes (label <> ": \n" <> pursMd printedExpr <> "\n\n" <> label' <> ": \n" <> printedExpr') (Just $ spanToRange ss)

      respondWithTypedExpr :: Maybe P.SourceSpan -> P.Expr -> P.SourceType -> HandlerM ()
      respondWithTypedExpr ss expr tipe = do
        void $
          expr & onChildExprs \e -> do
            pure e
        let printedType = prettyPrintTypeSingleLine tipe
            printedExpr = case expr of
              P.Op _ (P.Qualified _ op) -> P.runOpName op -- pretty printing ops ends in infinite loop
              _ -> dispayExprOnHover expr
        markdownRes (pursTypeStr printedExpr (Just printedType) []) (spanToRange <$> ss)

      respondWithTypeBinder :: P.SourceSpan -> P.Binder -> P.SourceType -> HandlerM ()
      respondWithTypeBinder ss binder tipe = do
        let printedType = prettyPrintTypeSingleLine tipe
        markdownRes (pursTypeStr (dispayBinderOnHover binder) (Just printedType) []) (Just $ spanToRange ss)

      respondWithModule :: P.SourceSpan -> P.ModuleName -> HandlerM ()
      respondWithModule ss modName = do
        docsMb <- readModuleDocs modName
        case docsMb of
          Just docs | Just comments <- Docs.modComments docs -> markdownRes comments (Just $ spanToRange ss)
          _ -> nullRes

      respondWithImport :: P.SourceSpan -> P.ModuleName -> Maybe P.DeclarationRef -> HandlerM ()
      respondWithImport ss importedModuleName (Just ref) = do
        let name = P.declRefName ref
            nameType = getImportRefNameType ref
        respondWithDeclInModule ss nameType importedModuleName (printName name)
      respondWithImport ss importedModuleName _ = respondWithModule ss importedModuleName

      handleLiteral :: P.SourceSpan -> P.Literal a -> HandlerM Bool
      handleLiteral ss = \case
        P.NumericLiteral (Left int) -> do
          markdownRes (pursTypeStr (show int) (Just "Int") []) (Just $ spanToRange ss)
          pure False
        P.NumericLiteral (Right n) -> do
          markdownRes (pursTypeStr (show n) (Just "Number") []) (Just $ spanToRange ss)
          pure False
        P.StringLiteral str -> do
          markdownRes (pursTypeStr (ellipsis 64 $ show str) (Just "String") []) (Just $ spanToRange ss)
          pure False
        P.CharLiteral ch -> do
          markdownRes (pursTypeStr (show ch) (Just "Char") []) (Just $ spanToRange ss)
          pure False
        P.BooleanLiteral b -> do
          markdownRes (pursTypeStr (show b) (Just "Boolean") []) (Just $ spanToRange ss)
          pure False
        _ -> pure True

  forLsp filePathMb \filePath -> do
    inferredRes <- inferExprViaTypeHole filePath startPos
    case inferredRes of
      Just (expr, ty) -> do
        let ss = P.exprSourceSpan expr
        respondWithTypedExpr ss expr ty
      Nothing -> do
        debugLsp "Inferred via type hole failed"
        nullRes

-- cacheOpenMb <- cachedRebuild filePath

-- forLsp cacheOpenMb \OpenFile {..} -> do
--   let everything = getEverythingAtPos (P.getModuleDeclarations ofModule) startPos
--   case head (apExprs everything) of
--     Just (ss, _, e) -> do
--       inferredRes <-  filePath e
--       case inferredRes of
--         Right ty -> respondWithTypedExpr ss e ty
--         Left err -> do
--           debugLsp $ "Error: " <> show err
--           nullRes
--     _ -> nullRes

-- let handlePos :: Types.Position -> HandlerM ()
--     handlePos pos = do
--       let everything = getEverythingAtPos (P.getModuleDeclarations ofModule) pos
--       debugLsp $ "pos: " <> show pos

--       case apImport everything of
--         Just (ss, importedModuleName, _, ref) -> do
--           debugLsp $ "Import: " <> show importedModuleName
--           respondWithImport ss importedModuleName ref
--         _ -> do
--           let exprs = apExprs everything
--               handleExpr expr = do
--                 case expr of
--                   (ss, _, P.Var _ (P.Qualified (P.ByModuleName modName) ident)) -> do
--                     debugLsp $ "Var: " <> show ident
--                     respondWithDeclInModule ss IdentNameType modName (P.runIdent ident)
--                     pure False
--                   (ss, _, P.Op _ (P.Qualified (P.ByModuleName modName) ident)) -> do
--                     debugLsp $ "Op: " <> show ident
--                     respondWithDeclInModule ss ValOpNameType modName (P.runOpName ident)
--                     pure False
--                   (ss, _, P.Constructor _ (P.Qualified (P.ByModuleName modName) ident)) -> do
--                     debugLsp $ "Dctor: " <> show ident
--                     respondWithDeclInModule ss DctorNameType modName (P.runProperName ident)
--                     pure False
--                   (ss, _, P.TypedValue _ tExpr ty) | not (generatedExpr tExpr) -> do
--                     respondWithTypedExpr ss tExpr ty
--                     pure False
--                   (ss, _, P.Literal _ lit) -> do
--                     handleLiteral ss lit
--                   _ -> pure True

--           debugLsp $ "exprs found: " <> show (length exprs)
--           noExprFound <- allM handleExpr exprs

--           debugLsp $ "No expr found: " <> show noExprFound
--           when noExprFound do
--             debugLsp $ showCounts everything
--             let decls = apDecls everything & sortDeclsBySize
--             void $
--               apDecls everything & allM \case
--                 P.BoundValueDeclaration sa _binder expr -> do
--                   debugLsp "BoundValueDeclaration"
--                   let ss = fst sa
--                       children = getChildExprs expr
--                   children & allM \e -> handleExpr (ss, True, e)
--                 P.BindingGroupDeclaration bindingGroup -> do
--                   debugLsp "BindingGroupDeclaration"
--                   NE.toList bindingGroup & allM \((sa, _), _, expr) ->
--                     getChildExprs expr & allM \child -> handleExpr (fst sa, True, child)
--                 decl@(P.ValueDeclaration vd) -> do
--                   debugLsp $ "ValueDeclaration IDENT: " <> P.runIdent (P.valdeclIdent vd)
--                   debugLsp $ "ValueDeclaration: " <> show vd
--                   let ss = P.declSourceSpan decl
--                       guaredExprs = P.valdeclExpression vd
--                       children = guaredExprs >>= getChildExprs . (\(P.GuardedExpr _ e) -> e)
--                   children & allM \expr ->
--                     handleExpr (ss, True, expr)
--                 decl -> do
--                   debugLsp $ "Decl: " <> ellipsis 100 (show decl)
--                   pure False
-- handlePos startPos

inferAtPosition :: FilePath -> Types.Position -> HandlerM (Maybe (Either P.MultipleErrors (P.SourceSpan, P.Expr, P.SourceType)))
inferAtPosition filePath pos@(Types.Position {..}) = do
  cacheOpenMb <- cachedRebuild filePath
  case cacheOpenMb of
    Nothing -> pure Nothing
    Just OpenFile {..} -> do
      let everything = getEverythingAtPos (P.getModuleDeclarations ofModule) pos

      case (apTopLevelDecl everything, head $ apExprs everything) of
        (Just decl, Just (ss, _, expr)) -> do
          let onDecl d = pure d
              onExpr e = do
                when (e == expr) do
                  (P.TypedValue' _ _ t) <- infer' e
                  tell $ P.MultipleErrors [P.ErrorMessage [] (P.HoleInferredType hoverHoleLabel t [] Nothing)]
                -- P.MultipleErrors  [P.ErrorMessage [] (P.HoleInferredType hoverHoleLabel inferred [] Nothing)]
                pure e
              onBinder b = do
                !_ <- pure $ force $ traceShow' "onBinder" b
                case b of
                  P.TypedBinder _st _b' -> pure ()
                  _ -> pure ()
                pure b

              (inferExpr, _, _) = P.everywhereOnValuesTopDownM onDecl onExpr onBinder

          -- runInference :: HandlerM (Either P.MultipleErrors (P.Declaration, P.MultipleErrors))
          -- runInference = runExceptT $ runWriterT $ evalSupplyT 0 $ evalStateT (inferExpr decl) ((P.emptyCheckState ofStartingEnv) {P.checkCurrentModule = Just ofModuleName})

          inferRes <- runInference (inferExpr decl) ofModuleName ofEndEnv
          case getHoverSourceTypeFromErrs inferRes of
            Just t -> pure $ Just $ Right (ss, expr, t)
            _ -> pure $ Just $ Left $ getResErrors inferRes
        _ -> pure Nothing
  where
    runInference a modName env =
      runExceptT $
        runWriterT $
          evalSupplyT 0 $
            evalStateT a ((P.emptyCheckState env) {P.checkCurrentModule = Just modName})

-- inferBinder :: P.SourceType -> P.Binder -> m (Map P.Ident (P.SourceSpan, P.SourceType))
-- inferBinder _ NullBinder = return M.empty
-- inferBinder val (LiteralBinder _ (StringLiteral _)) = unifyTypes val tyString >> return M.empty
-- inferBinder val (LiteralBinder _ (CharLiteral _)) = unifyTypes val tyChar >> return M.empty
-- inferBinder val (LiteralBinder _ (NumericLiteral (Left _))) = unifyTypes val tyInt >> return M.empty
-- inferBinder val (LiteralBinder _ (NumericLiteral (Right _))) = unifyTypes val tyNumber >> return M.empty
-- inferBinder val (LiteralBinder _ (BooleanLiteral _)) = unifyTypes val tyBoolean >> return M.empty
-- inferBinder val (VarBinder ss name) = return $ M.singleton name (ss, val)
-- inferBinder val (ConstructorBinder ss ctor binders) = do
--   env <- getEnv
--   case M.lookup ctor (P.dataConstructors env) of
--     Just (_, _, ty, _) -> do
--       let (args, ret) = peelArgs ty
--           expected = length args
--           actual = length binders
--       unifyTypes ret val
--       M.unions <$> zipWithM inferBinder (reverse args) binders
--     _ -> throwError . P.errorMessage' ss . P.UnknownName . fmap P.DctorName $ ctor
--   where
--     peelArgs :: P.Type a -> ([P.Type a], P.Type a)
--     peelArgs = go []
--       where
--         go args (P.TypeApp _ (P.TypeApp _ fn arg) ret) | P.eqType fn P.tyFunction = go (arg : args) ret
--         go args ret = (args, ret)
-- inferBinder _ _ = throwError "Not implemented"

getResErrors :: Either P.MultipleErrors (a, P.MultipleErrors) -> P.MultipleErrors
getResErrors = either identity snd

getHoverSourceTypeFromErrs :: Either P.MultipleErrors (P.Declaration, P.MultipleErrors) -> Maybe P.SourceType
getHoverSourceTypeFromErrs = \case
  Left (P.MultipleErrors errs) -> findMap getHoverHoleType errs
  Right (_, P.MultipleErrors errs) -> findMap getHoverHoleType errs

-- let everything = getEverythingAtPos (P.getModuleDeclarations ofModule) pos
-- case head (apExprs everything) of
--   Just (_, _, e) -> do
--     inferredRes <- inferExprType filePath e
--     case inferredRes of
--       Right ty -> pure $ Just (e, ty)
--       Left _ -> pure Nothing
--   _ -> pure Nothing

inferExprViaTypeHole :: FilePath -> Types.Position -> HandlerM (Maybe (P.Expr, P.SourceType))
inferExprViaTypeHole filePath pos = do
  cacheOpenMb <- cachedRebuild filePath
  cacheOpenMb & maybe (pure Nothing) \OpenFile {..} -> do
    let module' = P.importPrim ofUncheckedModule
        (moduleWithHole, exprs) = modifySmallestExprAtPos addTypeHoleAnnotation pos module'
    case exprs of
      Nothing -> pure Nothing
      Just (exprBefore, _exprAfter) -> do
        let externs = fmap edExtern ofDependencies
        (exportEnv, _) <- buildExportEnvCacheAndHandleErrors (selectDependencies module') module' externs
        (checkRes, warnings) <-
          runWriterT $
            runExceptT $
              P.desugarAndTypeCheck Nothing ofModuleName externs moduleWithHole exportEnv ofStartingEnv
        debugLsp $ "Infer via type hole checkRes: " <> show (isLeft checkRes)
        case checkRes of
          Right _ -> pure $ (exprBefore,) <$> findHoleType warnings
          Left errs -> do
            debugLsp $ T.pack $ P.prettyPrintMultipleErrors P.noColorPPEOptions (warnings <> errs)
            pure $
              (exprBefore,) <$> findHoleType (warnings <> errs)
  where
    findHoleType :: P.MultipleErrors -> Maybe P.SourceType
    findHoleType = P.runMultipleErrors >>> findMap getHoverHoleType

getHoverHoleType :: P.ErrorMessage -> Maybe P.SourceType
getHoverHoleType =
  P.unwrapErrorMessage >>> \case
    P.HoleInferredType label t _ _ | label == hoverHoleLabel -> Just t
    _ -> Nothing

findMap :: (a -> Maybe b) -> [a] -> Maybe b
findMap f = listToMaybe . mapMaybe f

-- addHoleAnnotation :: (Monad m) => P.Expr -> P.Declaration -> m P.Declaration
-- addHoleAnnotation expr = onDeclExprs \e ->
--   if e == expr
--     then
--       pure $
--         P.TypedValue False e (P.TypeWildcard P.nullSourceAnn $ P.HoleWildcard hoverHoleLabel)
--     else pure e

addTypeHoleAnnotation :: P.Expr -> P.Expr
addTypeHoleAnnotation expr = P.TypedValue False expr (P.TypeWildcard P.nullSourceAnn $ P.HoleWildcard hoverHoleLabel)

hoverHoleLabel :: Text
hoverHoleLabel = "?HOVER?"

onDeclExprs :: (Monad m) => (P.Expr -> m P.Expr) -> P.Declaration -> m P.Declaration
onDeclExprs fn = view _1 $ P.everywhereOnValuesTopDownM pure fn pure

onChildExprs :: (Monad m) => (P.Expr -> m P.Expr) -> P.Expr -> m P.Expr
onChildExprs fn = view _2 $ P.everywhereOnValuesTopDownM pure fn pure

allM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
allM _ [] = pure True
allM f (x : xs) = do
  b <- f x
  if b then allM f xs else pure False

generatedExpr :: P.Expr -> Bool
generatedExpr = \case
  P.Var _ ident -> generatedIdent $ P.disqualify ident
  P.Abs b e -> generatedBinder b || generatedExpr e
  P.App e e' -> generatedExpr e || generatedExpr e'
  P.TypedValue _ e _ -> generatedExpr e
  P.PositionedValue _ _ e -> generatedExpr e
  _ -> False

sortDeclsBySize :: [P.Declaration] -> [P.Declaration]
sortDeclsBySize = sortBy (compare `on` (spanSize . P.declSourceSpan))

traceToErr :: Text -> b -> b
traceToErr a b = trace a b

traceWith :: Text -> (b -> Text) -> b -> b
traceWith label f a = traceToErr (label <> ": " <> f a) a

traceShow' :: (Show b) => Text -> b -> b
traceShow' l = traceWith l show

generatedBinder :: P.Binder -> Bool
generatedBinder = \case
  P.VarBinder ss ident -> (ss == P.nullSourceSpan) || generatedIdent ident
  P.NamedBinder ss ident _ -> (ss == P.nullSourceSpan) || generatedIdent ident
  _ -> False

generatedIdent :: P.Ident -> Bool
generatedIdent = \case
  P.GenIdent {} -> True
  _ -> False

findTypedExpr :: [(P.SourceSpan, Bool, P.Expr)] -> Maybe (P.SourceSpan, P.Expr, P.SourceType)
findTypedExpr ((ss, _, P.TypedValue _ e t) : _) = Just (ss, e, t)
findTypedExpr (_ : es) = findTypedExpr es
findTypedExpr [] = Nothing

dispayExprOnHover :: P.Expr -> T.Text
dispayExprOnHover expr = ellipsis 32 $ T.strip $ T.pack $ render $ traceShow' "printed expr val" $ P.prettyPrintValue 2 expr

dispayBinderOnHover :: P.Binder -> T.Text
dispayBinderOnHover binder = line1Only $ ellipsis 32 $ T.strip $ P.prettyPrintBinder binder
  where
    line1Only = T.takeWhile (/= '\n')

ellipsis :: Int -> Text -> Text
ellipsis l t = if T.length t > l then T.take l t <> "..." else t

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

data InferError
  = FileNotCached
  | CompilationError P.MultipleErrors
  | InferException Text
  deriving (Show, Exception)

inferExprType :: FilePath -> P.Expr -> HandlerM (Either InferError P.SourceType)
inferExprType filePath expr = do
  cacheOpenMb <- cachedRebuild filePath
  case cacheOpenMb of
    Nothing -> pure $ Left FileNotCached
    Just OpenFile {..} -> do
      inferRes <- runWriterT $ runExceptT $ evalSupplyT 0 $ evalStateT (infer' expr) ((P.emptyCheckState ofStartingEnv) {P.checkCurrentModule = Just ofModuleName})
      pure $ bimap CompilationError (\(P.TypedValue' _ _ t) -> t) $ fst inferRes

inferExprType' :: FilePath -> P.Expr -> HandlerM P.SourceType
inferExprType' fp =
  inferExprType fp >=> \case
    Right t -> pure t
    Left e -> throwIO e

-- asdf =
-- ValueDeclaration
--   : ValueDeclarationData
--     { valdeclSourceAnn =
--         ( SourceSpan
--             { spanStart =
--                 SourcePos {sourcePosLine = 27, sourcePosColumn = 3},
--               spanEnd =
--                 SourcePos {sourcePosLine = 27, sourcePosColumn = 18}
--             },
--           []
--         ),
--       valdeclIdent = Ident "asdfa",
--       valdeclName = Public,
--       valdeclBinders = [],
--       valdeclExpression =
--         [ GuardedExpr
--             []
--             ( PositionedValue
--                 ( SourceSpan
--                     {         spanStart =
--                         SourcePos {sourcePosLine = 27, sourcePosColumn = 11},
--                       spanEnd =
--                         SourcePos {sourcePosLine = 27, sourcePosColumn = 18}
--                     }
--                 )
--                 []
--                 ( App
--                     ( App
--                         ( TypedValue
--                             True
--                             ( PositionedValue
--                                 ( SourceSpan
--                                     {                         spanStart =
--                                         SourcePos {sourcePosLine = 27, sourcePosColumn = 11},
--                                       spanEnd =
--                                         SourcePos {sourcePosLine = 27, sourcePosColumn = 15}
--                                     }
--                                 )
--                                 []
--                                 ( Var
--                                     ( SourceSpan
--                                         {                             spanStart =
--                                             SourcePos {sourcePosLine = 27, sourcePosColumn = 11},
--                                           spanEnd =
--                                             SourcePos {sourcePosLine = 27, sourcePosColumn = 15}
--                                         }
--                                     )
--                                     (Qualified (ByModuleName (ModuleName "Data.Show")) (Ident "show"))
--                                 )
--                             )
--                             ( ForAll
--                                 ( SourceSpan
--                                     { spanName = "",
--                                       spanStart =
--                                         SourcePos {sourcePosLine = 0, sourcePosColumn = 0},
--                                       spanEnd =
--                                         SourcePos {sourcePosLine = 0, sourcePosColumn = 0}
--                                     },
--                                   []
--                                 )
--                                 TypeVarVisible
--                                 "a"
--                                 ( Just
--                                     ( TypeConstructor
--                                         ( SourceSpan
--                                             { spanName = "",
--                                               spanStart =
--                                                 SourcePos {sourcePosLine = 0, sourcePosColumn = 0},
--                                               spanEnd =
--                                                 SourcePos {sourcePosLine = 0, sourcePosColumn = 0}
--                                             },
--                                           []
--                                         )
--                                         (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Type"}))
--                                     )
--                                 )
--                                 ( ConstrainedType
--                                     ( SourceSpan
--                                         { spanName = "",
--                                           spanStart =
--                                             SourcePos {sourcePosLine = 0, sourcePosColumn = 0},
--                                           spanEnd =
--                                             SourcePos {sourcePosLine = 0, sourcePosColumn = 0}
--                                         },
--                                       []
--                                     )
--                                     ( Constraint
--                                         { constraintAnn =
--                                             ( SourceSpan
--                                                 { spanName = "",
--                                                   spanStart =
--                                                     SourcePos {sourcePosLine = 0, sourcePosColumn = 0},
--                                                   spanEnd =
--                                                     SourcePos {sourcePosLine = 0, sourcePosColumn = 0}
--                                                 },
--                                               []
--                                             ),
--                                           constraintClass = Qualified (ByModuleName (ModuleName "Data.Show")) (ProperName {runProperName = "Show"}),
--                                           constraintKindArgs = [],
--                                           constraintArgs =
--                                             [ TypeVar
--                                                 ( SourceSpan
--                                                     { spanName = "",
--                                                       spanStart =
--                                                         SourcePos {sourcePosLine = 0, sourcePosColumn = 0},
--                                                       spanEnd =
--                                                         SourcePos {sourcePosLine = 0, sourcePosColumn = 0}
--                                                     },
--                                                   []
--                                                 )
--                                                 "a"
--                                             ],
--                                           constraintData = Nothing
--                                         }
--                                     )
--                                     ( TypeApp
--                                         ( SourceSpan {spanName = ".spago/p/prelude-6.0.1/src/Data/Show.purs", spanStart = SourcePos {sourcePosLine = 24, sourcePosColumn = 11}, spanEnd = SourcePos {sourcePosLine = 24, sourcePosColumn = 22}},
--                                           []
--                                         )
--                                         ( TypeApp
--                                             ( SourceSpan {spanName = ".spago/p/prelude-6.0.1/src/Data/Show.purs", spanStart = SourcePos {sourcePosLine = 24, sourcePosColumn = 11}, spanEnd = SourcePos {sourcePosLine = 24, sourcePosColumn = 22}},
--                                               []
--                                             )
--                                             ( TypeConstructor
--                                                 ( SourceSpan {spanName = ".spago/p/prelude-6.0.1/src/Data/Show.purs", spanStart = SourcePos {sourcePosLine = 24, sourcePosColumn = 13}, spanEnd = SourcePos {sourcePosLine = 24, sourcePosColumn = 15}},
--                                                   []
--                                                 )
--                                                 (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Function"}))
--                                             )
--                                             ( TypeVar
--                                                 ( SourceSpan {spanName = ".spago/p/prelude-6.0.1/src/Data/Show.purs", spanStart = SourcePos {sourcePosLine = 24, sourcePosColumn = 11}, spanEnd = SourcePos {sourcePosLine = 24, sourcePosColumn = 12}},
--                                                   []
--                                                 )
--                                                 "a"
--                                             )
--                                         )
--                                         ( TypeConstructor
--                                             ( SourceSpan {spanName = ".spago/p/prelude-6.0.1/src/Data/Show.purs", spanStart = SourcePos {sourcePosLine = 24, sourcePosColumn = 16}, spanEnd = SourcePos {sourcePosLine = 24, sourcePosColumn = 22}},
--                                               []
--                                             )
--                                             (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "String"}))
--                                         )
--                                     )
--                                 )
--                                 (Just (SkolemScope {runSkolemScope = 24}))
--                             )
--                         )
--                         ( Var
--                             ( SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}
--                             )
--                             (Qualified (ByModuleName (ModuleName "Data.Show")) (Ident "showInt"))
--                         )
--                     )
--                     ( TypedValue
--                         True
--                         ( TypedValue
--                             True
--                             ( PositionedValue
--                                 ( SourceSpan { spanStart = SourcePos {sourcePosLine = 27, sourcePosColumn = 16}, spanEnd = SourcePos {sourcePosLine = 27, sourcePosColumn = 18}}
--                                 )
--                                 []
--                                 ( Literal
--                                     ( SourceSpan { spanStart = SourcePos {sourcePosLine = 27, sourcePosColumn = 16}, spanEnd = SourcePos {sourcePosLine = 27, sourcePosColumn = 18}}
--                                     )
--                                     (NumericLiteral (Left 11))
--                                 )
--                             )
--                             ( TypeConstructor
--                                 ( SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}},
--                                   []
--                                 )
--                                 (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Int"}))
--                             )
--                         )
--                         ( TypeConstructor
--                             ( SourceSpan
--                                 { spanName = "",
--                                   spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0},
--                                   spanEnd =
--                                     SourcePos {sourcePosLine = 0, sourcePosColumn = 0}
--                                 },
--                               []
--                             )
--                             (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Int"}))
--                         )
--                     )
--                 )
--             )
--         ]
--     }