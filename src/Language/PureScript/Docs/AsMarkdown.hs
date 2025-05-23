module Language.PureScript.Docs.AsMarkdown
  ( Docs
  , runDocs
  , moduleAsMarkdown
  , codeToString
  , declAsMarkdown
  ) where

import Prelude

import Control.Monad (unless, zipWithM_)
import Control.Monad.Writer (Writer, tell, execWriter)

import Data.Foldable (for_)
import Data.List (partition)
import Data.Text (Text)
import Data.Text qualified as T

import Language.PureScript.Docs.RenderedCode (RenderedCode, RenderedCodeElement(..), outputWith)
import Language.PureScript.Docs.Types (ChildDeclaration(..), ChildDeclarationInfo(..), Declaration(..), Module(..), ignorePackage)
import Language.PureScript.Docs.Render qualified as Render
import Language.PureScript.Names qualified as P

moduleAsMarkdown :: Module -> Docs
moduleAsMarkdown Module{..} = do
  headerLevel 2 $ "Module " <> P.runModuleName modName
  spacer
  for_ modComments tell'
  mapM_ declAsMarkdown modDeclarations
  spacer
  for_ modReExports $ \(mn', decls) -> do
    let mn = ignorePackage mn'
    headerLevel 3 $ "Re-exported from " <> P.runModuleName mn <> ":"
    spacer
    mapM_ declAsMarkdown decls

declAsMarkdown :: Declaration -> Docs
declAsMarkdown decl@Declaration{..} = do
  headerLevel 4 (ticks declTitle)
  spacer

  let (instances, children) = partition (isChildInstance . cdeclInfo) declChildren
  fencedBlock $ do
    tell' (codeToString $ Render.renderDeclaration decl)
    zipWithM_ (\f c -> tell' (childToString f c)) (First : repeat NotFirst) children
  spacer

  for_ declComments tell'

  unless (null instances) $ do
    headerLevel 5 "Instances"
    fencedBlock $ mapM_ (tell' . childToString NotFirst) instances
    spacer

  where
  isChildInstance (ChildInstance _ _) = True
  isChildInstance _ = False

codeToString :: RenderedCode -> Text
codeToString = outputWith elemAsMarkdown
  where
  elemAsMarkdown (Syntax x)     = x
  elemAsMarkdown (Keyword x)    = x
  elemAsMarkdown Space          = " "
  elemAsMarkdown (Symbol _ x _) = x

  -- roles aren't rendered in markdown
  elemAsMarkdown (Role _) = ""

-- fixityAsMarkdown :: P.Fixity -> Docs
-- fixityAsMarkdown (P.Fixity associativity precedence) =
--   tell' $ concat [ "_"
--                  , associativityStr
--                  , " / precedence "
--                  , show precedence
--                  , "_"
--                  ]
--   where
--   associativityStr = case associativity of
--     P.Infixl -> "left-associative"
--     P.Infixr -> "right-associative"
--     P.Infix  -> "non-associative"

childToString :: First -> ChildDeclaration -> Text
childToString f decl@ChildDeclaration{..} =
  case cdeclInfo of
    ChildDataConstructor _ ->
      let c = if f == First then "=" else "|"
      in  "  " <> c <> " " <> str
    ChildTypeClassMember _ ->
      "  " <> str
    ChildInstance _ _ ->
      str
  where
  str = codeToString $ Render.renderChildDeclaration decl

data First
  = First
  | NotFirst
  deriving (Show, Eq, Ord)

type Docs = Writer [Text] ()

runDocs :: Docs -> Text
runDocs = T.unlines . execWriter

tell' :: Text -> Docs
tell' = tell . (:[])

spacer :: Docs
spacer = tell' ""

headerLevel :: Int -> Text -> Docs
headerLevel level hdr = tell' (T.replicate level "#" <> " " <> hdr)

fencedBlock :: Docs -> Docs
fencedBlock inner = do
  tell' "``` purescript"
  inner
  tell' "```"

ticks :: Text -> Text
ticks = ("`" <>) . (<> "`")
