{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as B
import Debug.Trace (trace)
import Data.Either
import Data.List
import Data.Maybe
import Data.Semigroup
import Foreign.C.Error
import System.Directory
import System.Exit
import System.Posix.Files
import System.Posix.IO
import System.Posix.Types
import System.Process
import Data.Text (Text)
import qualified Data.Text as T

import Dhall
import System.Fuse

type HT = ()

type Config = [BuildFile]

data BuildFile = BuildFile
  { name :: FilePath
  , dependencies :: [T.Text]
  , command :: T.Text
  } deriving (Generic, Show)

newtype BuildError =
  BuildError T.Text

instance Interpret BuildFile

configEntryByPath :: Config -> FilePath -> Maybe BuildFile
configEntryByPath config path = find matchingBuildFile config
  where
    matchingBuildFile :: BuildFile -> Bool
    matchingBuildFile (BuildFile name _ _) = ("/" <> name) == path

pathInConfig :: Config -> FilePath -> Bool
pathInConfig _ path
  | path == helloPath = True
pathInConfig config path = isJust $ configEntryByPath config path

-- when we want to access something about a file, we want to do something like this
-- is the current output up to date? a simplistic and dumb way to do this is to compare that file's mtime vs dep mtimes
-- if not, we need to rebuild the file before we can return a response
--
-- seems like we need a few stages then
--
-- FUSE OPS
--
--    |  ^
--    v  |
--
-- CACHE MANAGER (checks mtimes, stores output)
--
--    |  ^
--    v  |
--
-- BUILD MANAGER (runs commands, returns output
--
-- one first step could be changing it so that information about a buildfile is real
-- for example, just trying to get the length of the output requires us to have output
--
getOutput :: FilePath -> BuildFile -> IO (Either BuildError B.ByteString)
getOutput cwd (BuildFile name dependencies command) = do
  (code, stdout, stderr) <- readCreateProcessWithExitCode ((shell (T.unpack command)) {cwd = Just cwd}) ""
  return $ case code of
    ExitSuccess ->
      Right (B.pack (trace (stdout <> "\n" <> stderr) stdout))
    ExitFailure n ->
      Left $ BuildError $ "Error running command: " <> command <> ": " <> (T.pack . show) n <> " " <> (T.pack stderr)

main :: IO ()
main = do
  config <- input auto "./Packfile"
  currentDir <- getCurrentDirectory
  fuseMain (helloFSOps config currentDir) defaultExceptionHandler

helloFSOps :: Config -> FilePath -> FuseOperations HT
helloFSOps config cwd =
  defaultFuseOps
    { fuseGetFileStat = helloGetFileStat config cwd
    , fuseOpen = helloOpen config
    , fuseRead = helloRead config cwd
    , fuseOpenDirectory = helloOpenDirectory
    , fuseReadDirectory = helloReadDirectory config cwd
    , fuseGetFileSystemStats = helloGetFileSystemStats
    }

helloString :: B.ByteString
helloString = B.pack "Hello World, HFuse!\n"

helloPath :: FilePath
helloPath = "/hello"

dirStat :: FuseContext -> FileStat
dirStat ctx =
  FileStat
    { statEntryType = Directory
    , statFileMode =
        foldr1
          unionFileModes
          [ ownerReadMode
          , ownerExecuteMode
          , groupReadMode
          , groupExecuteMode
          , otherReadMode
          , otherExecuteMode
          ]
    , statLinkCount = 2
    , statFileOwner = fuseCtxUserID ctx
    , statFileGroup = fuseCtxGroupID ctx
    , statSpecialDeviceID = 0
    , statFileSize = 4096
    , statBlocks = 1
    , statAccessTime = 0
    , statModificationTime = 0
    , statStatusChangeTime = 0
    }

fileStat :: FuseContext -> FilePath -> BuildFile -> IO (Either BuildError FileStat)
fileStat ctx cwd buildFile = do
  output <- getOutput cwd buildFile
  return $ ((\o ->
      FileStat
        { statEntryType = RegularFile
        , statFileMode =
            foldr1 unionFileModes [ownerReadMode, groupReadMode, otherReadMode]
        , statLinkCount = 1
        , statFileOwner = fuseCtxUserID ctx
        , statFileGroup = fuseCtxGroupID ctx
        , statSpecialDeviceID = 0
        , statFileSize = fromIntegral $ B.length o
        , statBlocks = 1
        , statAccessTime = 0
        , statModificationTime = 0
        , statStatusChangeTime = 0
        }) <$>
   output)

helloGetFileStat :: Config -> FilePath -> FilePath -> IO (Either Errno FileStat)
helloGetFileStat _ _ "/" = do
  ctx <- getFuseContext
  return $ Right $ dirStat ctx
helloGetFileStat config cwd path =
  case configEntryByPath config path of
    Just buildFile -> do
      ctx <- getFuseContext
      eitherStat <- fileStat ctx cwd buildFile
      return $
        case eitherStat of
          Right fStat -> Right fStat
          Left _ -> Left eNOENT
    Nothing -> return $ Left eNOENT

helloOpenDirectory "/" = return eOK
helloOpenDirectory _ = return eNOENT

helloReadDirectory ::
     Config -> FilePath -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
helloReadDirectory config cwd "/" = do
  ctx <- getFuseContext
  bEntries <- buildEntries ctx
  let allEntries =
        [Right (".", dirStat ctx), Right ("..", dirStat ctx)] <> bEntries
  let errors = lefts allEntries
  let entries = rights allEntries
  return $ Right entries
  where
    (_:helloName) = helloPath
    buildEntries :: FuseContext -> IO [Either BuildError (FilePath, FileStat)]
    buildEntries ctx = Prelude.sequence $ buildEntry ctx <$> config
    buildEntry ctx b@(BuildFile name _ _) = do
      eitherStat <- fileStat ctx cwd b
      return $ (name,) <$> eitherStat
helloReadDirectory config _ _ = return (Left eNOENT)

helloOpen ::
     Config -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno HT)
helloOpen config path mode flags
  | pathInConfig config path =
    case mode of
      ReadOnly -> return (Right ())
      _ -> return (Left eACCES)
  | otherwise = return (Left eNOENT)

helloRead ::
     Config
  -> FilePath
  -> FilePath
  -> HT
  -> ByteCount
  -> FileOffset
  -> IO (Either Errno B.ByteString)
helloRead config cwd path _ byteCount offset =
  case configEntryByPath config path of
    Just buildFile -> do
      possibleOutput <- getOutput cwd buildFile
      case possibleOutput of
        Right o -> return $ Right $ B.take (fromIntegral byteCount) $ B.drop (fromIntegral offset) o
        Left _ -> return $ Left eNOENT
    Nothing -> return $ Left eNOENT

helloGetFileSystemStats :: String -> IO (Either Errno FileSystemStats)
helloGetFileSystemStats str =
  return $
  Right $
  FileSystemStats
    { fsStatBlockSize = 512
    , fsStatBlockCount = 1
    , fsStatBlocksFree = 1
    , fsStatBlocksAvailable = 1
    , fsStatFileCount = 5
    , fsStatFilesFree = 10
    , fsStatMaxNameLength = 255
    }
