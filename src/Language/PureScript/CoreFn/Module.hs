{-# LANGUAGE DeriveAnyClass #-}
module Language.PureScript.CoreFn.Module where

import Prelude

import Data.Map.Strict (Map)

import Language.PureScript.AST.SourcePos (SourceSpan)
import Language.PureScript.Comments (Comment)
import Language.PureScript.CoreFn.Expr (Bind)
import Language.PureScript.Names (Ident, ModuleName)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

-- |
-- The CoreFn module representation
--
data Module a = Module
  { moduleSourceSpan :: SourceSpan
  , moduleComments :: [Comment]
  , moduleName :: ModuleName
  , modulePath :: FilePath
  , moduleImports :: [(a, ModuleName)]
  , moduleExports :: [Ident]
  , moduleReExports :: Map ModuleName [Ident]
  , moduleForeign :: [Ident]
  , moduleDecls :: [Bind a]
  } deriving (Functor, Show, Generic, NFData)
