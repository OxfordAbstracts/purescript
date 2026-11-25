{-# LANGUAGE DeriveAnyClass #-}
module Language.PureScript.Make.HeaderCache
  ( headerCacheFile
  , parseModulesFromFiles
  ) where

import Prelude

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Control.Concurrent.Async.Chunked (forConcurrentlyChunked)
import Control.DeepSeq (NFData, force)
import Control.Exception (IOException, evaluate, try)
import Control.Monad (replicateM, unless)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Data.Array (Array, listArray, (!))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.List.NonEmpty (NonEmpty)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Calendar (Day(..))
import Data.Time.Clock (UTCTime(..), addUTCTime, diffTimeToPicoseconds, getCurrentTime)
import Data.Version (showVersion)
import GHC.Generics (Generic)
import Language.PureScript.AST (Declaration(..), ImportDeclarationType(..), Module(..), SourcePos(..), SourceSpan(..))
import Language.PureScript.CST qualified as CST
import Language.PureScript.Errors (MultipleErrors, parU)
import Language.PureScript.Make.Monad (makeIO)
import Language.PureScript.ModuleDependencies (ModuleSignature(..), moduleSignature)
import Language.PureScript.Names (ModuleName(..))
import Paths_purescript qualified as Paths
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, getModificationTime)
import System.FilePath ((</>))

data Timestamp = Timestamp !Int !Int
  deriving (Eq, Ord, Generic, NFData)

data CachedSpan = CachedSpan !Int !Int !Int !Int
  deriving (Eq, Generic, NFData)

data CachedImport = CachedImport !Text !CachedSpan
  deriving (Eq, Generic, NFData)

data CachedHeader = CachedHeader
  { chTimestamp :: !Timestamp
  , chSpan :: !CachedSpan
  , chModuleName :: !Text
  , chImports :: [CachedImport]
  } deriving (Generic, NFData)

headerCacheFile :: FilePath -> FilePath
headerCacheFile = (</> "cache-headers.cbor")

currentVersion :: Text
currentVersion = T.pack (showVersion Paths.version)

timestampSettlingSeconds :: Int
timestampSettlingSeconds = 2

toTimestamp :: UTCTime -> Timestamp
toTimestamp (UTCTime day time) =
  Timestamp (fromInteger (toModifiedJulianDay day)) (fromInteger (diffTimeToPicoseconds time))

encodeHeaderCache :: FilePath -> M.Map Text CachedHeader -> CBOR.Encoding
encodeHeaderCache cwd headers =
  CBOR.encodeString currentVersion
    <> CBOR.encodeString (T.pack cwd)
    <> CBOR.encodeInt (M.size nameIndices)
    <> foldMap CBOR.encodeString (M.keys nameIndices)
    <> CBOR.encodeInt (M.size headers)
    <> M.foldMapWithKey encodeHeader headers
  where
  nameIndices = M.fromDistinctAscList (zip (S.toAscList names) [0 :: Int ..])
  names = S.fromList
    [ name
    | CachedHeader{..} <- M.elems headers
    , name <- chModuleName : [ dep | CachedImport dep _ <- chImports ]
    ]

  encodeName = CBOR.encodeInt . (nameIndices M.!)

  encodeSpan (CachedSpan startLine startColumn endLine endColumn) =
    CBOR.encodeInt startLine <> CBOR.encodeInt startColumn <> CBOR.encodeInt endLine <> CBOR.encodeInt endColumn

  encodeHeader fileName CachedHeader{chTimestamp = Timestamp day picoseconds, ..} =
    CBOR.encodeString fileName
      <> CBOR.encodeInt day
      <> CBOR.encodeInt picoseconds
      <> encodeSpan chSpan
      <> encodeName chModuleName
      <> CBOR.encodeInt (length chImports)
      <> foldMap (\(CachedImport dep importSpan) -> encodeName dep <> encodeSpan importSpan) chImports

decodeHeaderCache :: FilePath -> CBOR.Decoder s (M.Map Text CachedHeader)
decodeHeaderCache cwd = do
  version <- CBOR.decodeString
  workingDirectory <- CBOR.decodeString
  if version /= currentVersion || workingDirectory /= T.pack cwd
    then pure M.empty
    else do
      nameCount <- CBOR.decodeInt
      names <- listArray (0, nameCount - 1) <$> replicateM nameCount CBOR.decodeString
      headerCount <- CBOR.decodeInt
      M.fromList <$> replicateM headerCount (decodeHeader nameCount names)
  where
  decodeName :: Int -> Array Int Text -> CBOR.Decoder s Text
  decodeName nameCount names = do
    index <- CBOR.decodeInt
    if index >= 0 && index < nameCount
      then pure (names ! index)
      else fail "name index out of range"

  decodeSpan = CachedSpan <$> CBOR.decodeInt <*> CBOR.decodeInt <*> CBOR.decodeInt <*> CBOR.decodeInt

  decodeHeader nameCount names = do
    fileName <- CBOR.decodeString
    timestamp <- Timestamp <$> CBOR.decodeInt <*> CBOR.decodeInt
    headerSpan <- decodeSpan
    moduleName <- decodeName nameCount names
    importCount <- CBOR.decodeInt
    imports <- replicateM importCount (CachedImport <$> decodeName nameCount names <*> decodeSpan)
    pure (fileName, CachedHeader timestamp headerSpan moduleName imports)

readHeaderCache :: FilePath -> FilePath -> IO (M.Map Text CachedHeader)
readHeaderCache outputDir cwd = do
  contents <- try (BS.readFile (headerCacheFile outputDir))
  pure $ case contents of
    Left (_ :: IOException) ->
      M.empty
    Right bytes ->
      either (const M.empty) snd $ CBOR.deserialiseFromBytes (decodeHeaderCache cwd) (BSL.fromStrict bytes)

writeHeaderCache :: FilePath -> FilePath -> M.Map Text CachedHeader -> IO ()
writeHeaderCache outputDir cwd headers = do
  createDirectoryIfMissing True outputDir
  BSL.writeFile (headerCacheFile outputDir) (CBOR.toLazyByteString (encodeHeaderCache cwd headers))

toCachedHeader :: Timestamp -> Module -> CachedHeader
toCachedHeader timestamp m =
  CachedHeader
    { chTimestamp = timestamp
    , chSpan = cachedSpan sigSourceSpan
    , chModuleName = name sigModuleName
    , chImports = [ CachedImport (name dep) (cachedSpan ss) | (dep, ss) <- sigImports ]
    }
  where
  ModuleSignature{..} = moduleSignature m
  cachedSpan (SourceSpan _ (SourcePos startLine startColumn) (SourcePos endLine endColumn)) =
    CachedSpan startLine startColumn endLine endColumn
  name (ModuleName text) = text

fromCachedHeader :: FilePath -> Text -> Text -> CachedHeader -> CST.PartialResult Module
fromCachedHeader path fileName content CachedHeader{..} =
  CST.PartialResult
    { CST.resPartial =
        Module (sourceSpan chSpan) [] (ModuleName chModuleName)
          [ ImportDeclaration (sourceSpan importSpan, []) (ModuleName dep) Implicit Nothing
          | CachedImport dep importSpan <- chImports
          ]
          Nothing
    , CST.resFull = content `seq` (rejectChangedHeader <$> CST.parseFromFile path content)
    }
  where
  sourceSpan (CachedSpan startLine startColumn endLine endColumn) =
    SourceSpan fileName (SourcePos startLine startColumn) (SourcePos endLine endColumn)

  rejectChangedHeader = \case
    Right m
      | CachedHeader _ _ moduleName imports <- toCachedHeader chTimestamp m
      , moduleName /= chModuleName || imports /= chImports ->
          Left (pure headerChanged)
    result -> result

  headerChanged = CST.ParserErrorInfo
    { CST.errRange = headerRange chSpan
    , CST.errToks = []
    , CST.errStack = []
    , CST.errType = CST.ErrCustom "The module name or imports changed while the modification time of the file stayed the same. Update the modification time of the file and build again."
    }

  headerRange (CachedSpan startLine startColumn endLine endColumn) =
    CST.SourceRange (CST.SourcePos startLine startColumn) (CST.SourcePos endLine endColumn)

type ParsedHeader = Either (NonEmpty CST.ParserError) (CST.PartialResult Module)

parseModulesFromFiles
  :: forall m
   . (MonadIO m, MonadError MultipleErrors m)
  => FilePath
  -> [(FilePath, Text)]
  -> m [(FilePath, CST.PartialResult Module)]
parseModulesFromFiles outputDir files = do
  parsed <- makeIO "read module headers" $ do
    now <- getCurrentTime
    cwd <- getCurrentDirectory
    cached <- readHeaderCache outputDir cwd
    let settledBefore = toTimestamp (addUTCTime (negate (fromIntegral timestampSettlingSeconds)) now)
    loaded <- forConcurrentlyChunked 64 files (loadHeader cached settledBefore)
    let headers = M.fromList [ (fileName, header) | (_, fileName, _, Just header, _) <- loaded ]
        allFromCache = and [ fromCache | (_, _, _, _, fromCache) <- loaded ]
    unless (allFromCache && M.size headers == M.size cached) $
      writeHeaderCache outputDir cwd headers
    pure [ (path, result) | (path, _, result, _, _) <- loaded ]
  parU parsed $ \(path, result) ->
    (path,) <$> CST.unwrapParserError path result
  where
  loadHeader
    :: M.Map Text CachedHeader
    -> Timestamp
    -> (FilePath, Text)
    -> IO (FilePath, Text, ParsedHeader, Maybe CachedHeader, Bool)
  loadHeader cached settledBefore (path, content) = do
    timestamp <- toTimestamp <$> getModificationTime path
    let fileName = T.pack path
    case M.lookup fileName cached of
      Just header | chTimestamp header == timestamp ->
        pure (path, fileName, Right (fromCachedHeader path fileName content header), Just header, True)
      _ -> do
        result <- evaluate (CST.parseModuleFromFile path content)
        case result of
          Left _ ->
            pure (path, fileName, result, Nothing, False)
          Right partial -> do
            header <- evaluate . force $ toCachedHeader timestamp (CST.resPartial partial)
            pure (path, fileName, result, if timestamp <= settledBefore then Just header else Nothing, False)
