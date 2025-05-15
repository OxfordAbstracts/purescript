module Language.PureScript.Lsp.ReadFile where

import Control.Monad.Catch (MonadThrow (throwM))
import Data.Text.Utf16.Rope.Mixed (Rope)
import Data.Text.Utf16.Rope.Mixed qualified as Rope
import Language.LSP.Protocol.Types (NormalizedUri)
import Language.LSP.Server (MonadLsp, getVirtualFile)
import Language.LSP.VFS qualified as VFS
import Language.PureScript.Lsp.ServerConfig (ServerConfig)
import Protolude

lspReadFileText ::
  (MonadThrow m, MonadLsp ServerConfig m) =>
  NormalizedUri ->
  m Text
lspReadFileText fp =
  Rope.toText <$> lspReadFileRope fp

lspReadFileRope ::
  (MonadThrow m, MonadLsp ServerConfig m) =>
  NormalizedUri ->
  m Rope
lspReadFileRope fp = do
  vfMb <- getVirtualFile fp
  case vfMb of
    Nothing -> throwM $ VirtualFileNotFoundException fp
    Just vf -> pure $ VFS._file_text vf

data VirtualFileNotFoundException = VirtualFileNotFoundException NormalizedUri
  deriving (Show)

instance Exception VirtualFileNotFoundException