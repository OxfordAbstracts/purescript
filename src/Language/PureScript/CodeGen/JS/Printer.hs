-- | Pretty printer for the JavaScript AST
module Language.PureScript.CodeGen.JS.Printer
  ( prettyPrintJS
  , prettyPrintJSWithSourceMaps
  ) where

import Prelude

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.List.NonEmpty qualified as NEL (toList)

import Language.PureScript.AST (SourceSpan)
import Language.PureScript.CodeGen.JS.Common (identCharToText, isValidJsIdentifier, nameIsJsBuiltIn, nameIsJsReserved)
import Language.PureScript.CoreImp.AST (AST(..), BinaryOperator(..), CIComments(..), UnaryOperator(..), getSourceSpan)
import Language.PureScript.CoreImp.Module (Export(..), Import(..), Module(..))
import Language.PureScript.Comments (Comment(..))
import Language.PureScript.Crash (internalError)
import Language.PureScript.Pretty.Common (Emit(..), SMap, StrPos(..), addMapping', blockIndent, intercalate, parensPos, runPlainString)
import Language.PureScript.PSString (PSString, decodeString, prettyPrintStringJS)

indentation :: (Emit gen) => Int -> gen
indentation i = emit $ T.replicate i " "

indexerLevel, accessorLevel, appLevel, newLevel, lamLevel, prefixLevel, instanceOfLevel, topLevel :: Int
indexerLevel = 1
accessorLevel = 2
appLevel = 3
newLevel = 4
lamLevel = 5
prefixLevel = 6
instanceOfLevel = 10
topLevel = 16

unaryLevel :: UnaryOperator -> Int
unaryLevel New = newLevel
unaryLevel _ = prefixLevel

binaryOperator :: BinaryOperator -> (Int, Text)
binaryOperator = \case
  Multiply -> (7, "*")
  Divide -> (7, "/")
  Modulus -> (7, "%")
  Add -> (8, "+")
  Subtract -> (8, "-")
  ShiftLeft -> (9, "<<")
  ShiftRight -> (9, ">>")
  ZeroFillShiftRight -> (9, ">>>")
  LessThan -> (10, "<")
  LessThanOrEqualTo -> (10, "<=")
  GreaterThan -> (10, ">")
  GreaterThanOrEqualTo -> (10, ">=")
  EqualTo -> (11, "===")
  NotEqualTo -> (11, "!==")
  BitwiseAnd -> (12, "&")
  BitwiseXor -> (13, "^")
  BitwiseOr -> (14, "|")
  And -> (15, "&&")
  Or -> (16, "||")

accessorProperty :: AST -> Maybe Text
accessorProperty (StringLiteral _ prop)
  | Just s <- decodeString prop, isValidJsIdentifier s = Just s
accessorProperty _ = Nothing

value :: (Emit gen) => Int -> Int -> AST -> gen
value level i js = case js of
  Indexer _ index val
    | level >= accessorLevel, Just prop <- accessorProperty index -> accessor i prop val
    | level >= indexerLevel -> indexer i index val
  App _ val args | level >= appLevel -> app i val args
  Unary _ op val | level >= unaryLevel op -> unary i op val
  Function ss name args ret | level >= lamLevel -> lam i ss name args ret
  Binary _ op v1 v2 | level >= fst (binaryOperator op) -> binary i op v1 v2
  InstanceOf _ v1 v2 | level >= instanceOfLevel -> instanceOf i v1 v2
  _ -> literal i js

accessor :: (Emit gen) => Int -> Text -> AST -> gen
accessor i prop val = operand <> emit "." <> emit prop
  where
  operand = case val of
    Indexer _ index' val' | Just prop' <- accessorProperty index' -> accessor i prop' val'
    _ -> value (accessorLevel - 1) i val

indexer :: (Emit gen) => Int -> AST -> AST -> gen
indexer i index val = operand <> emit "[" <> prettyPrintJS' i index <> emit "]"
  where
  operand = case val of
    Indexer _ index' val' -> indexer i index' val'
    _ -> value (indexerLevel - 1) i val

app :: (Emit gen) => Int -> AST -> [AST] -> gen
app i val args = operand <> emit "(" <> intercalate (emit ", ") (map (prettyPrintJS' i) args) <> emit ")"
  where
  operand = case val of
    App _ val' args' -> app i val' args'
    _ -> value (appLevel - 1) i val

unary :: (Emit gen) => Int -> UnaryOperator -> AST -> gen
unary i op val = emit str <> operand
  where
  str = case op of
    New -> "new "
    Not -> "!"
    BitwiseNot -> "~"
    Positive -> "+"
    Negate | isNegate val -> "- "
           | otherwise -> "-"
  isNegate (Unary _ Negate _) = True
  isNegate _ = False
  operand = case val of
    Unary _ op' val' | op' == op -> unary i op val'
    _ -> value (unaryLevel op - 1) i val

lam :: (Emit gen) => Int -> Maybe SourceSpan -> Maybe Text -> [Text] -> AST -> gen
lam i ss name args ret =
  addMapping' ss <>
    emit ("function "
      <> fromMaybe "" name
      <> "(" <> intercalate ", " args <> ") ")
      <> operand
  where
  operand = case ret of
    Function ss' name' args' ret' -> lam i ss' name' args' ret'
    _ -> value (lamLevel - 1) i ret

binary :: (Emit gen) => Int -> BinaryOperator -> AST -> AST -> gen
binary i op v1 v2 = left <> emit (" " <> str <> " ") <> value (level - 1) i v2
  where
  (level, str) = binaryOperator op
  left = case v1 of
    Binary _ op' v1' v2' | op' == op -> binary i op v1' v2'
    _ -> value (level - 1) i v1

instanceOf :: (Emit gen) => Int -> AST -> AST -> gen
instanceOf i v1 v2 = value (instanceOfLevel - 1) i v1 <> emit " instanceof " <> right
  where
  right = case v2 of
    InstanceOf _ v1' v2' -> instanceOf i v1' v2'
    _ -> value (instanceOfLevel - 1) i v2

literal :: (Emit gen) => Int -> AST -> gen
literal i js = case js of
  NumericLiteral _ n -> mapped $ emit $ T.pack $ either show show n
  StringLiteral _ s -> mapped $ emit $ prettyPrintStringJS s
  BooleanLiteral _ True -> mapped $ emit "true"
  BooleanLiteral _ False -> mapped $ emit "false"
  ArrayLiteral _ xs -> mapped $ mconcat
    [ emit "[ "
    , intercalate (emit ", ") (map (prettyPrintJS' i) xs)
    , emit " ]"
    ]
  ObjectLiteral _ [] -> mapped $ emit "{}"
  ObjectLiteral _ ps -> mapped $ mconcat
    [ emit "{\n"
    , intercalate (emit ",\n") $ map ((indentation inner <>) . property) ps
    , emit "\n"
    , indentation i
    , emit "}"
    ]
  Block _ sts -> mapped $ mconcat
    [ emit "{\n"
    , prettyStatements inner sts
    , emit "\n"
    , indentation i
    , emit "}"
    ]
  Var _ ident -> mapped $ emit ident
  VariableIntroduction _ ident val -> mapped $ mconcat
    [ emit $ "var " <> ident
    , maybe mempty ((emit " = " <>) . prettyPrintJS' i . snd) val
    ]
  Assignment _ target val -> mapped $ mconcat
    [ prettyPrintJS' i target
    , emit " = "
    , prettyPrintJS' i val
    ]
  While _ cond sts -> mapped $ mconcat
    [ emit "while ("
    , prettyPrintJS' i cond
    , emit ") "
    , prettyPrintJS' i sts
    ]
  For _ ident start end sts -> mapped $ mconcat
    [ emit $ "for (var " <> ident <> " = "
    , prettyPrintJS' i start
    , emit $ "; " <> ident <> " < "
    , prettyPrintJS' i end
    , emit $ "; " <> ident <> "++) "
    , prettyPrintJS' i sts
    ]
  ForIn _ ident obj sts -> mapped $ mconcat
    [ emit $ "for (var " <> ident <> " in "
    , prettyPrintJS' i obj
    , emit ") "
    , prettyPrintJS' i sts
    ]
  IfElse _ cond thens elses -> mapped $ mconcat
    [ emit "if ("
    , prettyPrintJS' i cond
    , emit ") "
    , prettyPrintJS' i thens
    , maybe mempty ((emit " else " <>) . prettyPrintJS' i) elses
    ]
  Return _ val -> mapped $ mconcat
    [ emit "return "
    , prettyPrintJS' i val
    ]
  ReturnNoResult _ -> mapped $ emit "return"
  Throw _ val -> mapped $ mconcat
    [ emit "throw "
    , prettyPrintJS' i val
    ]
  Comment (SourceComments com) js' -> mapped $ mconcat
    [ emit "\n"
    , mconcat $ map (comment i) com
    , prettyPrintJS' i js'
    ]
  Comment PureAnnotation js' -> mapped $ mconcat
    [ emit "/* #__PURE__ */ "
    , prettyPrintJS' i js'
    ]
  ModuleAccessor{} -> internalError "Incomplete pattern"
  _ -> parensPos $ prettyPrintJS' i js
  where
  mapped = (addMapping' (getSourceSpan js) <>)
  inner = i + blockIndent
  property (key, val) = (objectPropertyToString key <> emit ": ") <> prettyPrintJS' inner val
  objectPropertyToString :: (Emit gen) => PSString -> gen
  objectPropertyToString s =
    emit $ case decodeString s of
      Just s' | isValidJsIdentifier s' ->
        s'
      _ ->
        prettyPrintStringJS s

comment :: (Emit gen) => Int -> Comment -> gen
comment i (LineComment com) = mconcat
  [ indentation i
  , emit "//" <> emit com <> emit "\n"
  ]
comment i (BlockComment com) = mconcat $
  [ indentation i
  , emit "/**\n"
  ] ++
  map asLine (T.lines com) ++
  [ indentation i
  , emit " */\n"
  , indentation i
  ]
  where
  asLine s = indentation i <> emit " * " <> (emit . removeComments) s <> emit "\n"

  removeComments :: Text -> Text
  removeComments t =
    case T.stripPrefix "*/" t of
      Just rest -> removeComments rest
      Nothing -> case T.uncons t of
        Just (x, xs) -> x `T.cons` removeComments xs
        Nothing -> ""

prettyImport :: (Emit gen) => Import -> gen
prettyImport (Import ident from) =
  emit $
    "import * as " <> ident <> " from " <> prettyPrintStringJS from <> ";"

prettyExport :: (Emit gen) => Export -> gen
prettyExport (Export idents from) =
  mconcat
    [ emit "export {\n"
    , intercalate (emit ",\n") . NEL.toList $ (indentation blockIndent <>) . emit . exportedIdentToString from <$> idents
    , emit "\n"
    , emit $ "}" <> maybe "" ((" from " <>) . prettyPrintStringJS) from <> ";"
    ]
  where
  exportedIdentToString Nothing ident
    | nameIsJsReserved ident || nameIsJsBuiltIn ident
    = "$$" <> ident <> " as " <> ident
  exportedIdentToString _ "$main"
    = T.concatMap identCharToText "$main" <> " as $main"
  exportedIdentToString _ ident
    = T.concatMap identCharToText ident

prettyStatements :: (Emit gen) => Int -> [AST] -> gen
prettyStatements i sts =
  intercalate (emit "\n") $ map ((<> emit ";") . (indentation i <>) . prettyPrintJS' i) sts

prettyModule :: (Emit gen) => Module -> gen
prettyModule Module{..} =
  mconcat (map (comment 0) modHeader)
    <> intercalate (emit "\n") (map prettyImport modImports ++ prettyStatements 0 modBody : map prettyExport modExports)

-- | Generate a pretty-printed string representing a collection of JavaScript expressions at the same indentation level
prettyPrintJSWithSourceMaps :: Module -> (Text, [SMap])
prettyPrintJSWithSourceMaps js =
  let StrPos (_, s, mp) = prettyModule js
  in (s, mp)

prettyPrintJS :: Module -> Text
prettyPrintJS = runPlainString . prettyModule

-- | Generate an indented, pretty-printed string representing a JavaScript expression
prettyPrintJS' :: (Emit gen) => Int -> AST -> gen
prettyPrintJS' = value topLevel
