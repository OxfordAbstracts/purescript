module Language.PureScript.Ide.UsageSpec where

import Protolude

import Data.Text qualified as Text
import Language.PureScript.Ide.Command (Command(..))
import Language.PureScript.Ide.Types (IdeNamespace(..), Success(..))
import Language.PureScript.Ide.Test qualified as Test
import Language.PureScript qualified as P
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, shouldBe)
import Data.Text.Read (decimal)
import System.FilePath ((</>), makeRelative)

load :: [Text] -> Command
load = LoadSync . map Test.mn

usage :: P.ModuleName -> Text -> IdeNamespace -> Command
usage = FindUsages

shouldBeUsage :: P.SourceSpan -> (FilePath, Text) -> Expectation
shouldBeUsage usage' (fp, range) =
  let
    [ start, end] = Text.splitOn "-" range
    unsafeReadInt = fst . either (panic "") identity . decimal
    [ startLine, startColumn ] = map unsafeReadInt (Text.splitOn ":" start)
    [ endLine, endColumn ] = map unsafeReadInt (Text.splitOn ":" end)
  in
    do
      projectDir <- Test.getProjectDirectory
      makeRelative projectDir (P.spanName usage') `shouldBe` fp

      (P.sourcePosLine (P.spanStart usage'), P.sourcePosColumn (P.spanStart usage'))
        `shouldBe`
        (startLine, startColumn)

      (P.sourcePosLine (P.spanEnd usage'), P.sourcePosColumn (P.spanEnd usage'))
        `shouldBe`
        (endLine, endColumn)

spec :: Spec
spec = describe "Finding Usages" $ do
    it "finds a simple usage" $ do
      ([_, Right (UsagesResult [usage1, usage2])], _) <- Test.inProject $
        Test.runIde [ load ["FindUsage", "FindUsage.Definition", "FindUsage.Reexport"]
                    , usage (Test.mn "FindUsage.Definition") "usageId" IdeNSValue
                    ]
      usage1 `shouldBeUsage` ("src" </> "FindUsage" </> "Definition.purs", "13:18-13:25")
      usage2 `shouldBeUsage` ("src" </> "FindUsage.purs", "12:11-12:18")
    it "finds a simple recursive usage" $ do
      ([_, Right (UsagesResult [usage1])], _) <- Test.inProject $
        Test.runIde [ load ["FindUsage.Recursive"]
                    , usage (Test.mn "FindUsage.Recursive") "recursiveUsage" IdeNSValue
                    ]
      usage1 `shouldBeUsage` ("src" </> "FindUsage" </> "Recursive.purs", "7:12-7:26")
    it "finds all references including locally shadowed ones (limitation: doesn't filter by scope)" $ do
      ([_, Right (UsagesResult usageResult)], _) <- Test.inProject $
        Test.runIde [ load ["FindUsage.RecursiveShadowed"]
                    , usage (Test.mn "FindUsage.RecursiveShadowed") "recursiveUsage" IdeNSValue
                    ]
      -- Note: The SQLite-based implementation finds all textual references,
      -- including those shadowed by local bindings. Proper scope tracking would
      -- require additional complexity.
      length usageResult `shouldBe` 1
    it "finds a constructor usage" $ do
      ([_, Right (UsagesResult usages)], _) <- Test.inProject $
        Test.runIde [ load ["FindUsage", "FindUsage.Definition", "FindUsage.Reexport"]
                    , usage (Test.mn "FindUsage.Definition") "Used" IdeNSValue
                    ]
      case usages of
        (usage1:_) -> usage1 `shouldBeUsage` ("src" </> "FindUsage.purs", "8:3-8:9")
        [] -> expectationFailure "No constructor usages found"
    it "finds a constructor alias usage" $ do
      ([_, Right (UsagesResult usages)], _) <- Test.inProject $
        Test.runIde [ load ["FindUsage", "FindUsage.Definition", "FindUsage.Reexport"]
                    , usage (Test.mn "FindUsage.Definition") "$%" IdeNSValue
                    ]
      case usages of
        (usage1:_) -> usage1 `shouldBeUsage` ("src" </> "FindUsage.purs", "9:5-9:7")
        [] -> expectationFailure "No constructor usages found"
    it "finds a reexported usage" $ do
      ([_, Right (UsagesResult [usage1])], _) <- Test.inProject $
        Test.runIde [ load ["FindUsage", "FindUsage.Definition", "FindUsage.Reexport"]
                    , usage (Test.mn "FindUsage.Reexport") "toBeReexported" IdeNSValue
                    ]
      usage1 `shouldBeUsage` ("src" </> "FindUsage.purs", "12:19-12:33")
