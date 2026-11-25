module Control.Concurrent.Async.Chunked
  ( forConcurrentlyChunked
  ) where

import Prelude

import Control.Concurrent.Async.Lifted qualified as Async
import Control.Monad.Trans.Control (MonadBaseControl)

forConcurrentlyChunked :: MonadBaseControl IO m => Int -> [a] -> (a -> m b) -> m [b]
forConcurrentlyChunked chunkSize xs f =
  concat <$> Async.forConcurrently (chunksOf xs) (traverse f)
  where
  chunksOf ys = case splitAt chunkSize ys of
    ([], _) -> []
    (chunk, rest) -> chunk : chunksOf rest
