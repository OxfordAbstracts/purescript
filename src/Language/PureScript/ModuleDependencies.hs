-- | Provides the ability to sort modules based on module dependencies
module Language.PureScript.ModuleDependencies
  ( DependencyDepth(..)
  , sortModules
  , ModuleGraph
  , ModuleSignature(..)
  , moduleSignature
  ) where

import Protolude hiding (head)

import Data.Array (Array, (!), array, bounds, indices, listArray)
import Data.Graph (SCC(..), graphFromEdges, stronglyConnComp)
import Data.IntSet qualified as IS
import Data.Set qualified as S
import Language.PureScript.AST (Declaration(..), ErrorMessageHint(..), Module(..), SourceSpan)
import Language.PureScript.Constants.Prim qualified as C
import Language.PureScript.Crash (internalError)
import Language.PureScript.Errors (MultipleErrors, SimpleErrorMessage(..), addHint, errorMessage', errorMessage'', parU)
import Language.PureScript.Names (ModuleName)

-- | A list of modules with their transitive dependencies
type ModuleGraph = [(ModuleName, [ModuleName])]

-- | A module signature for sorting dependencies.
data ModuleSignature = ModuleSignature
  { sigSourceSpan :: SourceSpan
  , sigModuleName :: ModuleName
  , sigImports :: [(ModuleName, SourceSpan)]
  }

data DependencyDepth = Direct | Transitive

-- | Sort a collection of modules based on module dependencies.
--
-- Reports an error if the module graph contains a cycle.
sortModules
  :: forall m a
   . MonadError MultipleErrors m
  => DependencyDepth
  -> (a -> ModuleSignature)
  -> [a]
  -> m ([a], ModuleGraph)
sortModules dependencyDepth toSig ms = do
    let
      ms' = (\m -> (m, toSig m)) <$> ms
      mns = S.fromList $ map (sigModuleName . snd) ms'
    verts <- parU ms' (toGraphNode mns)
    ms'' <- parU (stronglyConnComp verts) toModule
    let (graph, _, toVertex) = graphFromEdges verts
        keys :: Array Int ModuleName
        keys = array (bounds graph) [(v, mn) | mn <- S.toList mns, Just v <- [toVertex mn]]
        transitiveDeps :: Array Int IS.IntSet
        transitiveDeps = listArray (bounds graph) [ IS.unions [ IS.insert d (transitiveDeps ! d) | d <- graph ! v ] | v <- indices graph ]
        moduleGraph = do (_, mn, _) <- verts
                         let v       = fromMaybe (internalError "sortModules: vertex not found") (toVertex mn)
                             deps    = case dependencyDepth of
                                         Direct -> graph ! v
                                         Transitive -> IS.toList (transitiveDeps ! v)
                             toKey = (keys !)
                         return (mn, filter (/= mn) (map toKey deps))
    keys `seq` graph `seq` return (fst <$> ms'', moduleGraph)
  where
    toGraphNode :: S.Set ModuleName -> (a, ModuleSignature) -> m ((a, ModuleSignature), ModuleName, [ModuleName])
    toGraphNode mns m@(_, ModuleSignature _ mn deps) = do
      void . parU deps $ \(dep, pos) ->
        when (dep `notElem` C.primModules && S.notMember dep mns) .
          throwError
            . addHint (ErrorInModule mn)
            . errorMessage' pos
            $ ModuleNotFound dep
      pure (m, mn, map fst deps)

-- | Calculate a list of used modules based on explicit imports and qualified names.
usedModules :: Declaration -> Maybe (ModuleName, SourceSpan)
-- Regardless of whether an imported module is qualified we still need to
-- take into account its import to build an accurate list of dependencies.
usedModules (ImportDeclaration (ss, _) mn _ _) = pure (mn, ss)
usedModules _ = Nothing

-- | Convert a strongly connected component of the module graph to a module
toModule :: MonadError MultipleErrors m => SCC (a, ModuleSignature) -> m (a, ModuleSignature)
toModule (AcyclicSCC m) = return m
toModule (CyclicSCC ms) =
  case nonEmpty ms of
    Nothing ->
      internalError "toModule: empty CyclicSCC"
    Just ms' ->
      throwError
        . errorMessage'' (fmap (sigSourceSpan . snd) ms')
        $ CycleInModules (map (sigModuleName . snd) ms')

moduleSignature :: Module -> ModuleSignature
moduleSignature (Module ss _ mn ds _) = ModuleSignature ss mn (ordNub (mapMaybe usedModules ds))
