{-# LANGUAGE RecordWildCards #-}

module FileSystem where

import System.Directory
import qualified System.Directory.Internal as SDI
import Data.Time.Clock
import qualified Data.ByteString as B
import System.FilePath.Posix

data Directory = File { curPath :: FilePath,
                        content :: B.ByteString,
                        curPermissions :: Permissions,
                        curType :: String,
                        modificationTime :: UTCTime,
                        curSize :: Integer
                      }
               | Directory { internal :: [Directory],
                             curPath :: FilePath,
                             curPermissions :: Permissions
                           }
instance Show Directory where
  show (File path cont perm typ modTime curSz)  = "File { curPath = " ++ show path ++ ", content = " ++ show cont ++
                                                  ", curPermissions = " ++ show perm ++ ", curType = " ++ show typ ++
                                                  ", curSize = " ++ show curSz ++ " }"
  show (Directory int path perm)                = "Directory { internal = " ++ show int ++ ", curPath = " ++
                                                  show path ++ ", curPermissions = " ++ show perm ++ " }"

--rootDir :: FilePath
--rootDir = "/home/galina13/Study/Haskell/fp-homework-templates-2020/TestFolder"

readDirectory :: FilePath -> IO Directory
readDirectory path = do
  isDir <- doesDirectoryExist path
  if isDir
  then do
    kids <- getInternal path --[FilePath]
    perms <- getPermissions path
    return $ Directory kids path perms
  else do
    let fileType = takeExtension path
    fileSize <- getFileSize path
    fileContent <- B.readFile path
    perms <- getPermissions path
    modTime <- getModificationTime path
    return $ File path fileContent perms fileType modTime fileSize

getInternal :: FilePath -> IO [Directory]
getInternal path = do
 intDirs <- listDirectory path --[FilePath]
 dirs <- sequenceA $ map readDirectory $ map (\name -> path ++ "/" ++ name) (intDirs)
 return $ dirs

isDirectory :: Directory -> Bool
isDirectory dir = case dir of
  File{..}      -> False
  Directory{..} -> True

defaultPermissions :: Permissions
defaultPermissions = SDI.Permissions True True False True