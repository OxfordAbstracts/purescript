module Command.Lsp (command) where

import Language.PureScript.Lsp.Types (mkEnv, LspLogLevel(..))
import Language.PureScript.Lsp as Lsp
import Options.Applicative qualified as Opts
import Protolude
import SharedCLI qualified
import System.Directory (setCurrentDirectory)

data ServerOptions = ServerOptions
  { _serverDirectory :: Maybe FilePath,
    _serverGlobs :: [FilePath],
    _serverGlobsFromFile :: Maybe FilePath,
    _serverGlobsExcluded :: [FilePath],
    _serverOutputPath :: FilePath,
    _serverLoglevel :: LspLogLevel
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
    server opts'@(ServerOptions dir _globs _globsFromFile _globsExcluded outputPath logLevel) = do
      when
        (logLevel == LogDebug || logLevel == LogAll)
        (hPutStrLn stderr ("Parsed Options:" :: Text) *> hPutStrLn stderr (show opts' :: Text))
      maybe (pure ()) setCurrentDirectory dir
      env <- mkEnv outputPath
      startServer env

    serverOptions :: Opts.Parser ServerOptions
    serverOptions =
      ServerOptions
        <$> optional (Opts.strOption (Opts.long "directory" `mappend` Opts.short 'd'))
        <*> many SharedCLI.inputFile
        <*> SharedCLI.globInputFile
        <*> many SharedCLI.excludeFiles
        <*> Opts.strOption (Opts.long "output-directory" `mappend` Opts.value "output/")
        <*> ( parseLogLevel
                <$> Opts.strOption
                  ( Opts.long "log-level"
                      `mappend` Opts.value ""
                      `mappend` Opts.help "One of \"debug\", \"perf\", \"all\" or \"none\""
                  )
            )

    parseLogLevel :: Text -> LspLogLevel
    parseLogLevel s = case s of
      "debug" -> LogDebug
      "perf" -> LogPerf
      "all" -> LogAll
      "none" -> LogNone
      _ -> LogWarning

    startServer env = do
      code <- Lsp.main env
      exitWith
        ( case code of
            0 -> ExitSuccess
            _ -> ExitFailure code
        )
