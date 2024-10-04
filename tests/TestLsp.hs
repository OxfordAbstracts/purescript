{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module TestLsp where

import Control.Concurrent.Async.Lifted (async, waitCatch)
import Control.Concurrent.STM (atomically, newTChan)
import Control.DeepSeq (force)
import Control.Exception (Exception (fromException), evaluate)
import Control.Lens ((^.))
import Control.Monad (void)
import Data.List (sort)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Distribution.Compat.CreatePipe (createPipe)
import GHC.IO.Exception (ExitCode (ExitSuccess))
import Language.LSP.Protocol.Lens (HasUri (uri))
import Language.LSP.Protocol.Types (ClientCapabilities, Definition (Definition), Location (Location), Position (Position), Range (Range), type (|?) (InL))
import Language.LSP.Server (runServer)
import Language.LSP.Test (Session, fullLatestClientCaps, getDefinitions, openDoc, runSession, runSessionWithConfig, SessionConfig (SessionConfig))
import Language.PureScript qualified as P
import Language.PureScript.Docs qualified as D
import Language.PureScript.Lsp (serverDefinition)
import Language.PureScript.Lsp.Types (LspConfig (LspConfig), LspLogLevel (LogError), mkEnv)
import Protolude hiding (Location)
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, it, shouldBe)

-- runPursLspSession ::

spec :: Spec
spec = describe "lsp" $ do
  it "should start" do
    runSessionWithConfig sessionConfig "purs lsp server" fullLatestClientCaps "tests/purs/lsp" do
      doc <- openDoc "Main.purs" "purs"
      defs2 <- getDefinitions doc (Position 2 0)
      defs3 <- getDefinitions doc (Position 3 0)
      defs4 <- getDefinitions doc (Position 4 0)
      let expRange = Range (Position 4 0) (Position 4 3)
      liftIO do
        defs2 `shouldBe` (InL $ Definition $ InL $ Location (doc ^. uri) expRange)
      pure ()


sessionConfig :: SessionConfig
sessionConfig = SessionConfig 10 False False True mempty True True True Nothing 

--   it "should run a test" $ do
--     "abc" `shouldBe` "abc"

-- runPursLspSession :: String -> ClientCapabilities -> FilePath -> Session b -> IO b
-- runPursLspSession testConfig caps root session = do
--   rin <- atomically newTChan
--   env <- mkEnv $ LspConfig "/output" ["."] LogError
--   server <- async $ void $ runServer $ serverDefinition env rin
--   res <- runSession testConfig caps root session
--   void $ timeout 3000000 $ do
--     Left (fromException -> Just ExitSuccess) <- waitCatch server
--     pure ()
--   pure res
