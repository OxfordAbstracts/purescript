{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Language.PureScript.Lsp.Handlers.Definition where

import Control.Lens (Field1 (_1), Field3 (_3), view, (^.))
import Control.Lens.Getter (to)
import Data.Map qualified as Map
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as Message
import Language.LSP.Protocol.Types qualified as Types
import Language.LSP.Server qualified as Server
import Language.LSP.VFS qualified as VFS
import Language.PureScript qualified as P
import Language.PureScript.AST qualified as AST
import Language.PureScript.AST.Traversals (everythingWithScopeAnn)
import Language.PureScript.CoreFn.Expr qualified as CF
import Language.PureScript.Lsp.Cache (selectExternModuleNameFromFilePath, selectExternPathFromModuleName)
import Language.PureScript.Lsp.Cache.Query (getAstDeclarationInModule, getCoreFnExprAt, getEfDeclarationInModule)
import Language.PureScript.Lsp.Docs (readQualifiedNameDocsSourceSpan)
import Language.PureScript.Lsp.Log (debugLsp)
import Language.PureScript.Lsp.Monad (HandlerM)
import Language.PureScript.Lsp.Print (printName)
import Language.PureScript.Lsp.State (cachedRebuild)
import Language.PureScript.Lsp.Types (OpenFile (..))
import Language.PureScript.Lsp.Util (declSourceSpanWithExpr, efDeclSourceSpan, getNamesAtPosition, posInSpan, posInSpanLines, sourcePosToPosition)
import Language.PureScript.Types (getAnnForType)
import Protolude hiding (to)
import Language.PureScript.Environment qualified as E

definitionHandler :: Server.Handlers HandlerM
definitionHandler = Server.requestHandler Message.SMethod_TextDocumentDefinition $ \req res -> do
  let Types.DefinitionParams docIdent pos _prog _prog' = req ^. LSP.params
      filePathMb = Types.uriToFilePath $ docIdent ^. LSP.uri

      nullRes = res $ Right $ Types.InR $ Types.InR Types.Null

      locationRes fp range = res $ Right $ Types.InL $ Types.Definition $ Types.InL $ Types.Location (Types.filePathToUri fp) range

      posRes fp srcPos = locationRes fp $ Types.Range (sourcePosToPosition srcPos) (sourcePosToPosition srcPos)

      sourceTypeLocRes st = locationRes (AST.spanName $ fst $ getAnnForType st) $ spanToRange $ fst $ getAnnForType st

      forLsp :: Maybe a -> (a -> HandlerM ()) -> HandlerM ()
      forLsp val f = maybe nullRes f val

  forLsp filePathMb \filePath -> do
    cacheOpenMb <- cachedRebuild filePath
    forLsp cacheOpenMb \OpenFile {..} -> do
      let withoutPrim =
            ofModule
              & P.getModuleDeclarations
              & filter (not . isPrimImport)

          declsAtPos =
            withoutPrim
              & filter (posInSpanLines pos . declSourceSpanWithExpr)

          decls = if null declsAtPos then withoutPrim else declsAtPos

          exprs = getExprsAtPos pos =<< decls

      case head exprs of
        Nothing -> nullRes
        Just expr -> do
          debugLsp $ "expr: " <> show expr
          case expr of
            P.Var _ (P.Qualified (P.BySourcePos srcPos) _) -> posRes filePath srcPos
            P.Var _ ident | Just (st, _ , _) <- Map.lookup ident (E.names ofFinalEnv) -> sourceTypeLocRes st
            P.Op _ (P.Qualified (P.BySourcePos srcPos) _) -> posRes filePath srcPos
            P.Constructor _ (P.Qualified (P.BySourcePos srcPos) _) -> posRes filePath srcPos
            P.Constructor _ ident | Just (_, _, st, _) <-  Map.lookup ident (E.dataConstructors ofFinalEnv) -> sourceTypeLocRes st
            P.VisibleTypeApp _ st -> sourceTypeLocRes st
            P.TypedValue _ _ st -> sourceTypeLocRes st
            _ -> nullRes
              -- do
              -- moduleNameMb <- selectExternModuleNameFromFilePath filePath
              -- forLsp moduleNameMb \moduleName' -> do
              --   nameMb <- getExprName moduleName expr
              --   forLsp nameMb \name -> do
              --     spanMb <- readQualifiedNameDocsSourceSpan name
              --     case spanMb of
              --       Just span -> locationRes (P.spanName span) (spanToRange span)
              --       _ -> nullRes
              -- forLsp (liftA2 (,) moduleNameMb (getExprName expr)) \(modName, name) -> do
              --   spanMb <- readQualifiedNameDocsSourceSpan name
              --   case spanMb of
              --     Just span -> locationRes (P.spanName span) (spanToRange span)
              --     _ -> nullRes

isPrimImport :: P.Declaration -> Bool
isPrimImport (P.ImportDeclaration _ (P.ModuleName "Prim") _ _) = True
isPrimImport (P.ImportDeclaration ss _ _ _) | ss == AST.nullSourceAnn = True
isPrimImport _ = False

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

getExprsAtPos' :: Types.Position -> P.Declaration -> [P.Expr]
getExprsAtPos' pos = fmap (view _1) . getExprsAndBindersAtPos' pos

getExprsAndBindersAtPos :: Types.Position -> P.Declaration -> [(P.Expr, [P.Declaration], [P.Binder])]
getExprsAndBindersAtPos pos declaration = view _3 $ execState (goDecl declaration) ([], [], [])
  where
    goDecl :: P.Declaration -> StateT ([P.Declaration], [P.Binder], [(P.Expr, [P.Declaration], [P.Binder])]) Identity P.Declaration
    goDecl = onDecl

    (onDecl, _, _) = P.everywhereOnValuesTopDownM handleDecl handleExpr handleBinder

    handleDecl :: P.Declaration -> StateT ([P.Declaration], [P.Binder], [(P.Expr, [P.Declaration], [P.Binder])]) Identity P.Declaration
    handleDecl decl = do
      modify $ \(decls, binds, exprs) -> (decl : decls, binds, exprs)
      pure decl

    handleExpr :: AST.Expr -> StateT ([P.Declaration], [P.Binder], [(P.Expr, [P.Declaration], [P.Binder])]) Identity AST.Expr
    handleExpr expr =
      if maybe False (posInSpan pos) (P.exprSourceSpan expr)
        then do
          modify $ \(decls, binds, exprs) -> (decls, binds, (expr, decls, binds) : exprs)
          pure expr
        else pure expr

    handleBinder :: AST.Binder -> StateT ([P.Declaration], [P.Binder], [(P.Expr, [P.Declaration], [P.Binder])]) Identity AST.Binder
    handleBinder binder = do
      modify $ first ((:) binder)
      pure binder

getExprsAndBindersAtPos' :: Types.Position -> P.Declaration -> [(P.Expr, [P.Declaration], [P.Binder])]
getExprsAndBindersAtPos' pos declaration = view _3 $ execState (goDecl declaration) ([], [], [])
  where
    goDecl :: P.Declaration -> StateT ([P.Declaration], [P.Binder], [(P.Expr, [P.Declaration], [P.Binder])]) Identity P.Declaration
    goDecl = onDecl

    (onDecl, _, _) = P.everywhereOnValuesM handleDecl handleExpr handleBinder

    handleDecl :: P.Declaration -> StateT ([P.Declaration], [P.Binder], [(P.Expr, [P.Declaration], [P.Binder])]) Identity P.Declaration
    handleDecl decl = do
      modify $ \(decls, binds, exprs) -> (decl : decls, binds, exprs)
      pure decl

    handleExpr :: AST.Expr -> StateT ([P.Declaration], [P.Binder], [(P.Expr, [P.Declaration], [P.Binder])]) Identity AST.Expr
    handleExpr expr =
      if maybe False (posInSpan pos) (P.exprSourceSpan expr)
        then do
          modify $ \(decls, binds, exprs) -> (decls, binds, (expr, decls, binds) : exprs)
          pure expr
        else pure expr

    handleBinder :: AST.Binder -> StateT ([P.Declaration], [P.Binder], [(P.Expr, [P.Declaration], [P.Binder])]) Identity AST.Binder
    handleBinder binder = do
      modify $ first ((:) binder)
      pure binder

getExprAtPosWithLocalBindAnn :: Types.Position -> P.Declaration -> [(P.Expr, P.Qualified P.Name, Maybe AST.SourceAnn)]
getExprAtPosWithLocalBindAnn pos decl = goDecl Map.empty decl
  where
    (goDecl, _, _, _, _) = everythingWithScopeAnn mempty handleExpr handleBinder mempty mempty

    handleExpr :: AST.IdentsAnn -> AST.Expr -> [(P.Expr, P.Qualified P.Name, Maybe AST.SourceAnn)]
    handleExpr idents expr = case expr of
      AST.Var ss i
        | posInSpan pos ss ->
            let lookupIdent locKind = Map.lookup (locKind $ P.disqualify i) idents
             in [(expr, P.IdentName <$> i, lookupIdent P.LocalIdent <|> lookupIdent P.ToplevelIdent)]
      AST.Constructor ss name
        | posInSpan pos ss ->
            [ (expr, P.DctorName <$> name, Nothing)
            ]
      AST.Op _ opName ->
        [ ( expr,
            P.ValOpName <$> opName,
            Nothing
          )
        ]
      _
        --   | Just ss <- exprSourceSpan expr, posInSpan pos ss ->
        --     [(expr, _ expr, Nothing)
        --     ]
        | otherwise -> []

    handleBinder :: AST.IdentsAnn -> AST.Binder -> [(P.Expr, P.Qualified P.Name, Maybe AST.SourceAnn)]
    handleBinder _idents binder = case binder of
      AST.ConstructorBinder ss ctorName _ ->
        [ ( AST.Constructor ss ctorName,
            P.DctorName <$> ctorName,
            Nothing
          )
        ]
      _ -> []

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

-- t = [ ImportDeclaration (SourceSpan { spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}
--   , spanEnd = SourcePos {sourcePosLine = 107, sourcePosColumn = 82}},[]) (ModuleName "Prim") Implicit (Just (ModuleName "Prim")),ImportDeclaration (SourceSpan { spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 107, sourcePosColumn = 82}},[]) (ModuleName "Prim") Implicit Nothing,ValueDeclaration (ValueDeclarationData {valdeclSourceAnn = (SourceSpan { spanStart = SourcePos {sourcePosLine = 75, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 77, sourcePosColumn = 8}},[]), valdeclIdent = Ident "v", valdeclName = Public, valdeclBinders = [], valdeclExpression = [GuardedExpr [] (TypedValue True (PositionedValue (SourceSpan { spanStart = SourcePos {sourcePosLine = 75, sourcePosColumn = 5}, spanEnd = SourcePos {sourcePosLine = 75, sourcePosColumn = 6}}) [] (Let FromWhere [ValueDeclaration (ValueDeclarationData {valdeclSourceAnn = (SourceSpan { spanStart = SourcePos {sourcePosLine = 77, sourcePosColumn = 3}, spanEnd = SourcePos {sourcePosLine = 77, sourcePosColumn = 8}},[]), valdeclIdent = Ident "a", valdeclName = Public, valdeclBinders = [], valdeclExpression = [GuardedExpr [] (PositionedValue (SourceSpan { spanStart = SourcePos {sourcePosLine = 77, sourcePosColumn = 7}, spanEnd = SourcePos {sourcePosLine = 77, sourcePosColumn = 8}}) [] (Literal (SourceSpan { spanStart = SourcePos {sourcePosLine = 77, sourcePosColumn = 7}, spanEnd = SourcePos {sourcePosLine = 77, sourcePosColumn = 8}}) (NumericLiteral (Left 1))))]})] (TypedValue True (PositionedValue (SourceSpan { spanStart = SourcePos {sourcePosLine = 75, sourcePosColumn = 5}, spanEnd = SourcePos {sourcePosLine = 75, sourcePosColumn = 6}}) [] (Var (SourceSpan { spanStart = SourcePos {sourcePosLine = 75, sourcePosColumn = 5}, spanEnd = SourcePos {sourcePosLine = 75, sourcePosColumn = 6}}) (Qualified (BySourcePos (SourcePos {sourcePosLine = 77, sourcePosColumn = 3})) (Ident "a")))) (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}},[]) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Int"})))))) (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}},[]) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Int"}))))]})]

-- x =
--   [ ValueDeclaration
--       ( ValueDeclarationData
--           { valdeclSourceAnn =
--               ( SourceSpan
--                   { spanStart = SourcePos {sourcePosLine = 75, sourcePosColumn = 1},
--                     spanEnd = SourcePos {sourcePosLine = 77, sourcePosColumn = 8}
--                   },
--                 []
--               ),
--             valdeclIdent = Ident "v",
--             valdeclName = Public,
--             valdeclBinders = [],
--             valdeclExpression =
--               [ GuardedExpr
--                   []
--                   ( TypedValue
--                       True
--                       ( PositionedValue
--                           ( SourceSpan {spanStart = SourcePos {sourcePosLine = 75, sourcePosColumn = 5}, spanEnd = SourcePos {sourcePosLine = 75, sourcePosColumn = 6}}
--                           )
--                           []
--                           ( Let
--                               FromWhere
--                               [ ValueDeclaration
--                                   ( ValueDeclarationData
--                                       { valdeclSourceAnn = (SourceSpan {spanStart = SourcePos {sourcePosLine = 77, sourcePosColumn = 3}, spanEnd = SourcePos {sourcePosLine = 77, sourcePosColumn = 8}}, []),
--                                         valdeclIdent = Ident "a",
--                                         valdeclName = Public,
--                                         valdeclBinders = [],
--                                         valdeclExpression =
--                                           [ GuardedExpr
--                                               []
--                                               ( PositionedValue
--                                                   ( SourceSpan
--                                                       { spanStart = SourcePos {sourcePosLine = 77, sourcePosColumn = 7},
--                                                         spanEnd = SourcePos {sourcePosLine = 77, sourcePosColumn = 8}
--                                                       }
--                                                   )
--                                                   []
--                                                   ( Literal
--                                                       ( SourceSpan
--                                                           { spanStart = SourcePos {sourcePosLine = 77, sourcePosColumn = 7},
--                                                             spanEnd = SourcePos {sourcePosLine = 77, sourcePosColumn = 8}
--                                                           }
--                                                       )
--                                                       (NumericLiteral (Left 1))
--                                                   )
--                                               )
--                                           ]
--                                       }
--                                   )
--                               ]
--                               ( TypedValue
--                                   True
--                                   ( PositionedValue
--                                       ( SourceSpan
--                                           { spanStart = SourcePos {sourcePosLine = 75, sourcePosColumn = 5},
--                                             spanEnd = SourcePos {sourcePosLine = 75, sourcePosColumn = 6}
--                                           }
--                                       )
--                                       []
--                                       (Var (SourceSpan {spanStart = SourcePos {sourcePosLine = 75, sourcePosColumn = 5}, spanEnd = SourcePos {sourcePosLine = 75, sourcePosColumn = 6}}) (Qualified (BySourcePos (SourcePos {sourcePosLine = 77, sourcePosColumn = 3})) (Ident "a")))
--                                   )
--                                   (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Int"})))
--                               )
--                           )
--                       )
--                       (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Int"})))
--                   )
--               ]
--           }
--       )
--   ]