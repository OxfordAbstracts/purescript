module Command.Lsp (command) where

import Language.PureScript.Lsp as Lsp
import Language.PureScript.Lsp.Types (mkEnv)
import Options.Applicative qualified as Opts
import Protolude
import System.Directory (setCurrentDirectory)

data ServerOptions = ServerOptions
  { _serverDirectory :: Maybe FilePath,
    _serverOutputPath :: FilePath
  }
  deriving (Show)

command :: Opts.Parser (IO ())
command = Opts.helper <*> subcommands
  where
    subcommands :: Opts.Parser (IO ())
    subcommands =
      (Opts.subparser . fold)
        [ Opts.command
            "server"
            ( Opts.info
                (fmap server serverOptions <**> Opts.helper)
                (Opts.progDesc "Start a server LSP process")
            )
        ]

    server :: ServerOptions -> IO ()
    server (ServerOptions dir outputPath) = do
      maybe (pure ()) setCurrentDirectory dir
      putErrLn $ "Starting server with output path: " <> outputPath
      env <- mkEnv outputPath
      startServer outputPath env

    serverOptions :: Opts.Parser ServerOptions
    serverOptions =
      ServerOptions
        <$> optional (Opts.strOption (Opts.long "directory" `mappend` Opts.short 'd'))
        <*> Opts.strOption (Opts.long "output-directory" `mappend` Opts.value "output/")

    startServer outputPath env = do
      code <- Lsp.main outputPath env
      exitWith
        ( case code of
            0 -> ExitSuccess
            _ -> ExitFailure code
        )
