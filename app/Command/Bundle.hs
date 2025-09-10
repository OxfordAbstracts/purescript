-- | Bundles compiled PureScript modules for the browser.
module Command.Bundle (command, initSqlite) where

import Prelude

import Language.PureScript.Make.IdeCache (sqliteInit)
import Options.Applicative qualified as Opts
import System.Exit (exitFailure)
import System.IO (stderr, hPutStrLn)


data PublishOptionsCLI = PublishOptionsCLI
  { cliCompileOutputDir :: FilePath
  }

compileOutputDir :: Opts.Parser FilePath
compileOutputDir = Opts.option Opts.auto $
     Opts.value "output"
  <> Opts.showDefault
  <> Opts.long "compile-output"
  <> Opts.metavar "DIR"
  <> Opts.help "Compiler output directory"

cliOptions :: Opts.Parser PublishOptionsCLI
cliOptions =
  PublishOptionsCLI <$> compileOutputDir

app :: IO ()
app = do
  hPutStrLn stderr $ unlines
    [ "'purs bundle' was removed in the v0.15.0 release."
    , "See https://github.com/purescript/documentation/blob/master/migration-guides/0.15-Migration-Guide.md"
    , "for more information and bundler alternatives."
    ]
  exitFailure

-- | Make it go.
command :: Opts.Parser (IO ())
command = run <$> (Opts.helper <*> pure ()) where
  run :: () -> IO ()
  run _ = app

initSqlite :: Opts.Parser (IO ())
initSqlite = run <$> (Opts.helper <*> cliOptions) where
  run :: PublishOptionsCLI -> IO ()
  run opts = do
    sqliteInit opts.cliCompileOutputDir
