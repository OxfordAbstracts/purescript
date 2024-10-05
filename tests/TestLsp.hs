{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module TestLsp (spec) where

import Control.Concurrent.Async.Lifted (async, waitCatch)
import Control.Concurrent.STM (atomically, newTChan)
import Control.DeepSeq (force)
import Control.Exception (Exception (fromException), evaluate, throw)
import Control.Lens ((^.))
import Control.Monad (void)
import Data.Aeson qualified as A
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List (sort)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Text qualified as Text
import Distribution.Compat.CreatePipe (createPipe)
import GHC.IO.Exception (ExitCode (ExitSuccess))
import Language.LSP.Protocol.Lens (HasUri (uri))
import Language.LSP.Protocol.Lens qualified as L
import Language.LSP.Protocol.Message (SMethod (SMethod_CustomMethod))
import Language.LSP.Protocol.Types (ClientCapabilities, Definition (Definition), Location (Location), Position (Position), Range (Range), type (|?) (InL))
import Language.LSP.Server (runServer)
import Language.LSP.Test (Session, SessionConfig (SessionConfig), SessionException (UnexpectedResponseError), fullLatestClientCaps, getDefinitions, openDoc, request, runSession, runSessionWithConfig)
import Language.PureScript qualified as P
import Language.PureScript.Docs qualified as D
import Language.PureScript.Ide.Filter.Declaration qualified as A
import Language.PureScript.Lsp (serverDefinition)
import Language.PureScript.Lsp.Types (LspConfig (LspConfig), LspLogLevel (LogError), mkEnv)
import Protolude hiding (Location)
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, it, shouldBe)

-- runPursLspSession ::

spec :: Spec
spec =
  it "should get definitions" do
    runSessionWithConfig sessionConfig ("purs lsp server " <> globs) fullLatestClientCaps "tests/purs/lsp" do
      void rebuildReq
      doc <- openDoc "Main.purs" "purs"
      defsAtLine4 <- getDefinitions doc (Position 4 1)
      let expRange = Range (Position 4 0) (Position 4 24)
      liftIO do
        defsAtLine4 `shouldBe` InL (Definition $ InL $ Location (doc ^. uri) expRange)
      pure ()
  where
    rebuildReq = do
      void $ request (SMethod_CustomMethod $ Proxy @"delete output") A.Null
      rsp <- request (SMethod_CustomMethod $ Proxy @"build") A.Null
      liftIO $ do
        print "got build response"
        print rsp
      case rsp ^. L.result of
        Right x -> pure x
        Left err -> throw $ UnexpectedResponseError (fromJust $ rsp ^. L.id) err

sessionConfig :: SessionConfig
sessionConfig = SessionConfig 30 True True True clientConfig True True True Nothing
  where
    clientConfig :: KeyMap A.Value
    clientConfig = KeyMap.singleton "oa-purescript-lsp" (A.toJSON pursLspConfig)

    pursLspConfig :: Map Text.Text A.Value
    pursLspConfig = Map.empty

globs :: [Char]
globs = prelude <> " " <> srcGlob

prelude :: [Char]
prelude = "tests/support/bower_components/purescript-prelude/src/**/*.purs"

srcGlob :: [Char]
srcGlob = "tests/purs/lsp/**/*.purs"