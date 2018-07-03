{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Dhall

import qualified Data.ByteString.Char8 as B
import Data.List (find)
import Data.Maybe (isJust)
import Data.Text.Lazy (pack, unpack)
import Foreign.C.Error
import System.Posix.Files
import System.Posix.IO
import System.Posix.Types

import System.Fuse

type Config = [BuildFile]

data BuildFile = BuildFile
  { name :: Text
  , dependencies :: [Text]
  , command :: Text
  } deriving (Generic, Show)

instance Interpret BuildFile

type HT = ()

main :: IO ()
main = do
  config <- input auto "./Packfile"
  fuseMain (helloFSOps config) defaultExceptionHandler

helloFSOps :: Config -> FuseOperations HT
helloFSOps config =
  defaultFuseOps
    { fuseGetFileStat = helloGetFileStat config
    , fuseOpen = helloOpen config
    , fuseRead = helloRead config
    , fuseOpenDirectory = helloOpenDirectory
    , fuseReadDirectory = helloReadDirectory
    , fuseGetFileSystemStats = helloGetFileSystemStats config
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

fileStat :: BuildFile -> FuseContext -> FileStat
fileStat buildFile ctx =
  FileStat
    { statEntryType = RegularFile
    , statFileMode =
        foldr1 unionFileModes [ownerReadMode, groupReadMode, otherReadMode]
    , statLinkCount = 1
    , statFileOwner = fuseCtxUserID ctx
    , statFileGroup = fuseCtxGroupID ctx
    , statSpecialDeviceID = 0
    , statFileSize = fromIntegral $ B.length helloString
    , statBlocks = 1
    , statAccessTime = 0
    , statModificationTime = 0
    , statStatusChangeTime = 0
    }

pathInConfig :: Text -> Config -> Maybe BuildFile
pathInConfig path config = find (\b -> name b == path) config

helloGetFileStat :: Config -> FilePath -> IO (Either Errno FileStat)
helloGetFileStat _ "/" = do
  ctx <- getFuseContext
  return $ Right $ dirStat ctx
helloGetFileStat config path =
  case pathInConfig config path of
    Just buildFile -> do
      ctx <- getFuseContext
      stat <- fileStat buildFile ctx
      Right $ 
    Nothing -> return $ Left eNOENT

helloOpenDirectory "/" = return eOK
helloOpenDirectory _ = return eNOENT

helloReadDirectory :: FilePath -> IO (Either Errno [(FilePath, FileStat)])
helloReadDirectory "/" = do
  ctx <- getFuseContext
  return $
    Right [(".", dirStat ctx), ("..", dirStat ctx), (helloName, fileStat ctx)]
  where
    (_:helloName) = helloPath
helloReadDirectory _ = return (Left (eNOENT))

helloOpen ::
     Config -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno HT)
helloOpen config path mode flags
  | path == helloPath =
    case mode of
      ReadOnly -> return (Right ())
      _ -> return (Left eACCES)
  | otherwise = return (Left eNOENT)

helloRead ::
     Config
  -> FilePath
  -> HT
  -> ByteCount
  -> FileOffset
  -> IO (Either Errno B.ByteString)
helloRead config path _ byteCount offset
  | path == helloPath =
    return $
    Right $
    B.take (fromIntegral byteCount) $ B.drop (fromIntegral offset) helloString
  | otherwise = return $ Left eNOENT

helloGetFileSystemStats :: Config -> String -> IO (Either Errno FileSystemStats)
helloGetFileSystemStats config str =
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
