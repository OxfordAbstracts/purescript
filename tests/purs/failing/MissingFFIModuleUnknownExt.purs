-- @shouldFailWith MissingFFIModule
module Main where

foreign import greeting :: String

main = greeting
