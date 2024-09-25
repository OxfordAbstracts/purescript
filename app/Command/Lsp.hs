module Command.Lsp (command) where

import Language.PureScript.Ide.Types (IdeLogLevel (..))
import Language.PureScript.Lsp.Types (LspConfig (..), mkEnv)
import Language.PureScript.LspSimple as Lsp
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
    _serverLoglevel :: IdeLogLevel
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
                (Opts.progDesc "Start a server process")
            )
        ]

    server :: ServerOptions -> IO ()
    server opts'@(ServerOptions dir globs _globsFromFile _globsExcluded outputPath logLevel) = do
      when
        (logLevel == LogDebug || logLevel == LogAll)
        (putText "Parsed Options:" *> print opts')
      maybe (pure ()) setCurrentDirectory dir
      let conf =
            LspConfig
              { confOutputPath = outputPath,
                confGlobs = globs,
                confLogLevel = logLevel
              }
      env <- mkEnv conf
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

    parseLogLevel :: Text -> IdeLogLevel
    parseLogLevel s = case s of
      "debug" -> LogDebug
      "perf" -> LogPerf
      "all" -> LogAll
      "none" -> LogNone
      _ -> LogDefault

    startServer env = do
      code <- Lsp.main env
      exitWith
        ( case code of
            0 -> ExitSuccess
            _ -> ExitFailure code
        )
