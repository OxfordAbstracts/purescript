-- @ffiExts ts
module Main where

import Prelude
import Effect.Console (log)

foreign import functionName :: String -> String

main = log "Done"
