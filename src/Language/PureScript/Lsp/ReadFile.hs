module Language.PureScript.Lsp.ReadFile where

import Control.Monad.Catch (MonadThrow (throwM))
import GHC.IO.Exception (IOException (ioe_description))
import Protolude hiding
  ( decodeUtf8,
    encodeUtf8,
    to,
  )
import System.Directory (makeAbsolute)
import System.IO.UTF8 (readUTF8FileT)


lspReadFile ::
  (MonadIO m, MonadThrow m) =>
  FilePath ->
  m (FilePath, Text)
lspReadFile fp = do
  absPath <-
    liftIO (try (makeAbsolute fp)) >>= \case
      Left (err :: IOException) ->
        throwM
          (err {ioe_description = "Couldn't resolve path for: " <> show fp <> ", " <> ioe_description err})
      Right absPath -> pure absPath
  contents <-
    liftIO (try (readUTF8FileT absPath)) >>= \case
      Left (err :: IOException) ->
        throwM
          (err {ioe_description = "Couldn't resolve path for: " <> show fp <> ", " <> ioe_description err})
      Right contents ->
        pure contents
  pure (absPath, contents)