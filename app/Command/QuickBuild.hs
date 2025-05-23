-----------------------------------------------------------------------------
--
-- Module      : Main
-- Description : The server accepting commands for psc-ide
-- Copyright   : Christoph Hegemann 2016
-- License     : MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  : Christoph Hegemann <christoph.hegemann1337@gmail.com>
-- Stability   : experimental
--
-- |
-- The server accepting commands for psc-ide
-----------------------------------------------------------------------------

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}

module Command.QuickBuild (command) where

import Protolude

import Data.Aeson qualified as Aeson
import Data.Set qualified as Set
import Control.Concurrent.STM (newTVarIO)
import "monad-logger" Control.Monad.Logger (MonadLogger, logDebug, logError, logInfo)
import Data.IORef (newIORef)
import Data.Text.IO qualified as T
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as BSL8
import GHC.IO.Exception (IOErrorType(..), IOException(..))
import Language.PureScript.Ide (handleCommand)
import Language.PureScript.Ide.Command (Command(..), commandName)
import Language.PureScript.Ide.Util (decodeT, displayTimeSpec, encodeT, logPerf, runLogger)
import Language.PureScript.Ide.Error (IdeError(..))
import Language.PureScript.Ide.State (updateCacheTimestamp)
import Language.PureScript.Ide.Types (Ide, IdeConfiguration(..), IdeEnvironment(..), IdeLogLevel(..), emptyIdeState)
import Network.Socket qualified as Network
import Options.Applicative qualified as Opts
import SharedCLI qualified
import System.Directory (doesDirectoryExist, getCurrentDirectory, setCurrentDirectory)
import System.FilePath ((</>))
import System.IO (BufferMode(..), hClose, hFlush, hSetBuffering, hSetEncoding, utf8)
import System.IO.Error (isEOFError)
import Database.SQLite.Simple qualified as SQLite
import  Language.PureScript.Options as PO

listenOnLocalhost :: Network.PortNumber -> IO Network.Socket
listenOnLocalhost port = do
  let hints = Network.defaultHints
        { Network.addrFamily = Network.AF_INET
        , Network.addrSocketType = Network.Stream
        }
  addr:_ <- Network.getAddrInfo (Just hints) (Just "127.0.0.1") (Just (show port))
  bracketOnError
    (Network.socket (Network.addrFamily addr) (Network.addrSocketType addr) (Network.addrProtocol addr))
    Network.close
    (\sock -> do
      Network.setSocketOption sock Network.ReuseAddr 1
      Network.bind sock (Network.addrAddress addr)
      Network.listen sock Network.maxListenQueue
      pure sock)

data ServerOptions = ServerOptions
  { _serverDirectory  :: Maybe FilePath
  , _serverGlobs      :: [FilePath]
  , _serverGlobsFromFile :: Maybe FilePath
  , _serverGlobsExcluded :: [FilePath]
  , _serverOutputPath :: FilePath
  , _srcFile :: FilePath
  , _serverPort       :: Network.PortNumber
  , _serverLoglevel   :: IdeLogLevel
  -- TODO(Christoph) Deprecated
  , _serverEditorMode :: Bool
  , _serverPolling    :: Bool
  , _serverNoWatch    :: Bool

  } deriving (Show)

data ClientOptions = ClientOptions
  { clientPort :: Network.PortNumber
  }

command :: Opts.Parser (IO ())
command = Opts.helper <*> subcommands where
  subcommands :: Opts.Parser (IO ())
  subcommands = (Opts.subparser . fold)
    [ Opts.command "server"
        (Opts.info (fmap server serverOptions <**> Opts.helper)
          (Opts.progDesc "Start a server process"))
    ]

  server :: ServerOptions -> IO ()
  server opts'@(ServerOptions dir globs globsFromFile globsExcluded outputPath srcFile port logLevel editorMode polling noWatch) = do
    when (logLevel == LogDebug || logLevel == LogAll)
      (putText "Parsed Options:" *> print opts')
    maybe (pure ()) setCurrentDirectory dir
    ideState <- newTVarIO emptyIdeState
    cwd <- getCurrentDirectory
    let fullOutputPath = cwd </> outputPath


    when noWatch
      (putText "The --no-watch flag is deprecated and ignored. purs ide no longer uses a file system watcher, instead it relies on its clients to notify it about updates and checks timestamps to invalidate itself")

    unlessM (doesDirectoryExist fullOutputPath) $ do
      putText "Your output directory didn't exist. This usually means you didn't compile your project yet."
      putText "psc-ide needs you to compile your project (for example by running pulp build)"

    let
      conf = IdeConfiguration
        { confLogLevel = logLevel
        , confOutputPath = outputPath
        , sqliteFilePath = outputPath </> "cache.db"
        , confGlobs = globs
        , confGlobsFromFile = globsFromFile
        , confGlobsExclude = globsExcluded
        }
    ts <- newIORef Nothing
    let
      env = IdeEnvironment
        { ideStateVar = ideState
        , ideConfiguration = conf
        , ideCacheDbTimestamp = ts
        , query = \q -> SQLite.withConnection (outputPath </> "cache.db")
             (\conn -> SQLite.query_ conn $ SQLite.Query q)
        }
    startServer srcFile env

  serverOptions :: Opts.Parser ServerOptions
  serverOptions =
    ServerOptions
      <$> optional (Opts.strOption (Opts.long "directory" `mappend` Opts.short 'd'))
      <*> many SharedCLI.inputFile
      <*> SharedCLI.globInputFile
      <*> many SharedCLI.excludeFiles
      <*> Opts.strOption (Opts.long "output-directory" `mappend` Opts.value "output/")
      <*> Opts.strOption (Opts.long "file" `mappend` Opts.value "output/")
      <*> (fromIntegral <$>
           Opts.option Opts.auto (Opts.long "port" `mappend` Opts.short 'p' `mappend` Opts.value (4242 :: Integer)))
      <*> (parseLogLevel <$> Opts.strOption
           (Opts.long "log-level"
            `mappend` Opts.value ""
            `mappend` Opts.help "One of \"debug\", \"perf\", \"all\" or \"none\""))
      -- TODO(Christoph): Deprecated
      <*> Opts.switch (Opts.long "editor-mode")
      <*> Opts.switch (Opts.long "no-watch")
      <*> Opts.switch (Opts.long "polling")

  parseLogLevel :: Text -> IdeLogLevel
  parseLogLevel s = case s of
    "debug" -> LogDebug
    "perf" -> LogPerf
    "all" -> LogAll
    "none" -> LogNone
    _ -> LogDefault

startServer :: FilePath -> IdeEnvironment -> IO ()
startServer fp'' env = do
  -- BSL8.putStrLn $ Aeson.encode fp''
  runLogger (confLogLevel (ideConfiguration env)) (runReaderT (rebuildC fp'') env)
  -- runLogger (confLogLevel (ideConfiguration env)) (runReaderT (forever (loop sock)) env)
  where
    rebuildC :: (Ide m, MonadLogger m) => FilePath -> m ()
    rebuildC fp = do
     runExceptT $ do
      result <- handleCommand (RebuildSync fp Nothing (Set.fromList [PO.JS]))

      -- liftIO $ BSL8.putStrLn $ Aeson.encode result 
       
      return ()


     return ()
    
    loop :: (Ide m, MonadLogger m) => Network.Socket -> m ()
    loop sock = do
      accepted <- runExceptT (acceptCommand sock)
      case accepted of
        Left err -> $(logError) err
        Right (cmd, h) -> do
          case decodeT cmd of
            Right cmd' -> do
              let message duration =
                    "Command "
                      <> commandName cmd'
                      <> " took "
                      <> displayTimeSpec duration
              logPerf message $ do
                result <- runExceptT $ do
                  updateCacheTimestamp >>= \case
                    Nothing -> pure ()
                    Just (before, after) -> do
                      -- If the cache db file was changed outside of the IDE
                      -- we trigger a reset before processing the command
                      $(logInfo) ("cachedb was changed from: " <> show before <> ", to: " <> show after)
                      unless (isLoadAll cmd') $
                        void (handleCommand Reset *> handleCommand (LoadSync []))
                  handleCommand cmd'
                liftIO $ catchGoneHandle $ BSL8.hPutStrLn h $ case result of
                  Right r  -> Aeson.encode r
                  Left err -> Aeson.encode err
              liftIO (hFlush stdout)
            Left err -> do
              let errMsg = "Parsing the command failed with:\n" <> err <> "\nCommand: " <> cmd
              $(logError) errMsg
              liftIO $ do
                catchGoneHandle (T.hPutStrLn h (encodeT (GeneralError errMsg)))
                hFlush stdout
          liftIO $ catchGoneHandle (hClose h)

isLoadAll :: Command -> Bool
isLoadAll = \case
  Load [] -> True
  _ -> False

catchGoneHandle :: IO () -> IO ()
catchGoneHandle =
  handle (\e -> case e of
    IOError { ioe_type = ResourceVanished } ->
      putText "[Error] psc-ide-server tried to interact with the handle, but the connection was already gone."
    _ -> throwIO e)

acceptCommand
  :: (MonadIO m, MonadLogger m, MonadError Text m)
  => Network.Socket
  -> m (Text, Handle)
acceptCommand sock = do
  h <- acceptConnection
  $(logDebug) "Accepted a connection"
  cmd' <- liftIO (catchJust
                  -- this means that the connection was
                  -- terminated without receiving any input
                  (\e -> if isEOFError e then Just () else Nothing)
                  (Just <$> T.hGetLine h)
                  (const (pure Nothing)))
  case cmd' of
    Nothing -> throwError "Connection was closed before any input arrived"
    Just cmd -> do
      $(logDebug) ("Received command: " <> cmd)
      pure (cmd, h)
  where
   acceptConnection = liftIO $ do
     -- Use low level accept to prevent accidental reverse name resolution
     (s,_) <- Network.accept sock
     h     <- Network.socketToHandle s ReadWriteMode
     hSetEncoding h utf8
     hSetBuffering h LineBuffering
     pure h
