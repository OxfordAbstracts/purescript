-- |
-- Desugaring passes
module Language.PureScript.Sugar (desugar, desugarUsingDb, module S) where

import Control.Category ((>>>))
import Control.Monad.Supply.Class (MonadSupply)
import Control.Monad.Writer.Class (MonadWriter)
import Data.Map qualified as M
import Language.PureScript.AST (Module)
import Language.PureScript.Environment (Environment)
import Language.PureScript.Environment qualified as P
import Language.PureScript.Errors (MultipleErrors)
import Language.PureScript.Externs (ExternsFile, ExternsFixity, ExternsTypeFixity)
import Language.PureScript.Linter.Imports (UsedImports)
import Language.PureScript.Names qualified as P
import Language.PureScript.Sugar.AdoNotation as S
import Language.PureScript.Sugar.BindingGroups as S
import Language.PureScript.Sugar.CaseDeclarations as S
import Language.PureScript.Sugar.DoNotation as S
import Language.PureScript.Sugar.LetPattern as S
import Language.PureScript.Sugar.Names as S
import Language.PureScript.Sugar.ObjectWildcards as S
import Language.PureScript.Sugar.Operators as S
import Language.PureScript.Sugar.TypeClasses as S
import Language.PureScript.Sugar.TypeClasses.Deriving as S
import Language.PureScript.Sugar.TypeDeclarations as S
import Protolude

-- |
-- The desugaring pipeline proceeds as follows:
--
--  * Remove signed literals in favour of `negate` applications
--
--  * Desugar object literals with wildcards into lambdas
--
--  * Desugar operator sections
--
--  * Desugar do-notation
--
--  * Desugar ado-notation
--
--  * Desugar top-level case declarations into explicit case expressions
--
--  * Desugar type declarations into value declarations with explicit type annotations
--
--  * Qualify any unqualified names and types
--
--  * Rebracket user-defined binary operators
--
--  * Introduce newtypes for type class dictionaries and value declarations for instances
--
--  * Group mutually recursive value and data declarations into binding groups.
desugar ::
  (MonadSupply m) =>
  (MonadError MultipleErrors m) =>
  (MonadWriter MultipleErrors m) =>
  (MonadState (Env, UsedImports) m) =>
  [ExternsFile] ->
  Module ->
  m Module
desugar externs =
  desugarSignedLiterals
    >>> desugarObjectConstructors
    >=> desugarDoModule
    >=> desugarAdoModule
    >=> desugarLetPatternModule
    >>> desugarCasesModule
    >=> desugarTypeDeclarationsModule
    >=> desugarImports
    >=> rebracket externs
    >=> checkFixityExports
    >=> deriveInstances
    >=> desugarTypeClasses externs
    >=> createBindingGroupsModule

desugarUsingDb ::
  (MonadSupply m) =>
  (MonadWriter MultipleErrors m) =>
  (MonadError MultipleErrors m) =>
  (MonadState (Env, UsedImports) m) =>
  [(P.ModuleName, [ExternsFixity])] ->
  [(P.ModuleName, [ExternsTypeFixity])] ->
  Environment ->
  Module ->
  m Module
desugarUsingDb fixities typeFixities env =
  desugarSignedLiterals
    >>> desugarObjectConstructors
    >=> desugarDoModule
    >=> desugarAdoModule
    >=> desugarLetPatternModule
    >>> desugarCasesModule
    >=> desugarTypeDeclarationsModule
    >=> desugarImports
    >=> rebracketFixitiesOnly (const True) fixities typeFixities
    >=> checkFixityExports
    >=> deriveInstances
    >=> desugarTypeClassesUsingMemberMap typeClassData
    >=> createBindingGroupsModule
  where
    typeClassData =
      P.typeClasses env
        & M.toList
        & mapMaybe addModuleName
        & M.fromList

addModuleName ::
  (P.Qualified (P.ProperName 'P.ClassName), P.TypeClassData) ->
  Maybe ((P.ModuleName, P.ProperName 'P.ClassName), P.TypeClassData)
addModuleName = \case
  (P.Qualified (P.ByModuleName mn) pn, tcd) -> Just ((mn, pn), tcd)
  _ -> Nothing
