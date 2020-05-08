{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Commands
  ( cd
  , ls
  , createFolder
  , createFile
  , cat
  , writeToFile
  , remove
  , findTheFile
  , information
  , createCvs
  , cvsInit
  , cvsAdd
  , cvsUpdate
  , showHelp
  , rewriteFs
  , cvsRemoveVersion
  , cvsRemove
  , cvsMerge
  , cvsHistory
  , cvsHistoryAll
  , getDir
  , cvsCat
  , App
  , AppState (..)
  ) where

import Control.Monad.State
import Control.Monad.Except
import FileSystem
import System.FilePath.Posix
import System.Directory
import ParserOptions
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import FileSystem
import Data.Time.Clock
import Data.Either
import Control.Monad.Catch
import qualified Control.Exception as CE

data AppState = AppState {
      fileSystem :: Directory,
      curDirectory :: Directory
    } deriving Show

type App =  ExceptT String (StateT AppState IO)

curDirPath :: AppState -> FilePath
curDirPath appState = curPath $ curDirectory appState

cd :: PathOptions -> App ()
cd (PathOptions path) = do
  appState <- lift get
  if (path == "..")
  then do
   isRoot <- isRootPath $ curDirPath appState
   if isRoot
   then throwError "Сurrent directory is root"
   else cd $ PathOptions $ takeDirectory $ curPath $ curDirectory appState
  else do
    if (isRelative path)
    then cdAbsolute $ PathOptions $ toAbsolute (curDirectory appState) path
    else cdAbsolute $ PathOptions $ path
    `catchError` (findDirectoryHandler path)

cdAbsolute :: PathOptions -> App () -- -> (Either String [String], AppState)
cdAbsolute (PathOptions path') = do
  let path = dropExtension path'
  dir <- findDirectoryFromRoot path
  lift $ modify (\curState -> curState {curDirectory = dir})

goToDirectory :: Directory -> FilePath -> App Directory
goToDirectory curDir path = if (equalFilePath (curPath curDir) path)
                            then return curDir
                            else do
                              dir <- nextDir
                              goToDirectory dir path
  where
    nextDir = intDir curDir $ head $ splitDirectories (makeRelative (curPath curDir) path)

intDir :: Directory -> FilePath -> App Directory
intDir curDir name = if (null resultAll)
                     then throwError "Nothing found"
                     else
                      if (null resultDirs)
                      then throwError "Not a directory"
                      else return $ head resultAll
  where
    resultAll = filter (\el -> (takeBaseName (curPath el) == name))
                                     (internal curDir)
    resultDirs = filter (\el -> (takeBaseName (curPath el) == name) && (isDirectory el))
                                     (internal curDir)


getDir :: NoOptions -> App [String]
getDir _ = do
  appState <- lift get
  intByDirectory $ curDirectory appState

intByDirectory :: Directory -> App [String]
intByDirectory curDir = return $ map takeBaseName $ map curPath $ internal $ curDir


ls :: PathOptions -> App [String]
ls (PathOptions path') = do
  let path = dropExtension path'
  absolutePath <- getAbsolutePath path
  dir <- findDirectoryFromRoot absolutePath
  intByDirectory dir
  `catchError` (findDirectoryHandler path')

toAbsolute :: Directory -> FilePath -> FilePath
toAbsolute dir path = joinPath [(curPath $ dir), path]

createFolder :: NameOptions -> App ()
createFolder (NameOptions name) = do
  appState <- lift get
  let curDir = curDirectory appState
  let newCurDir = curDir{internal = (Directory [] ((curPath curDir) ++ "/" ++ name) defaultPermissions) : (internal curDir)}
  updateFs newCurDir

createFile :: NameOptions -> App ()
createFile (NameOptions name) = do
  appState <- lift get
  let curDir = curDirectory appState
  curTime <- liftIO $ getCurrentTime
  let newFile = File { curPath = ((curPath curDir) ++ "/" ++ name),
                       content = B8.pack "",
                       curPermissions = defaultPermissions,
                       curType = takeExtension name,
                       modificationTime = curTime,
                       curSize = 0
                     }
  let newCurDir = curDir{internal = newFile : (internal curDir)}
  updateFs newCurDir

updateFs :: Directory -> App ()
updateFs newDir = do
  let path = curPath newDir
  appState <- lift get
  newFs <- updateDir path newDir (fileSystem appState)
  newCurDir <- goToDirectory newFs $ curDirPath appState
  lift $ modify $ (\curState -> curState {fileSystem = newFs, curDirectory = newCurDir})

updateDir :: FilePath -> Directory -> Directory -> App Directory
updateDir path newDir curDir = do
  if (equalFilePath (curPath curDir) path)
  then return newDir
  else do
    if (isDirectory curDir)
    then do
      let getNewInternal = sequenceA $ map (updateDir path newDir) (internal curDir)
      newInternal <- getNewInternal --[ExceptT .. Directory] => ExceptT .. [Directory] => [Directory]
      return curDir{internal = newInternal}
    else return curDir

cat :: PathOptions -> App B.ByteString
cat (PathOptions path) = do
  absolutePath <- getAbsolutePath path
  dir <- findDirectoryFromRoot (takeDirectory absolutePath)
  file <- getFile dir (takeBaseName path)
  return (content file)
  `catchError` (findTheFileHandler path)

getFile :: Directory -> FilePath -> App Directory
getFile curDir name =  if (null resultAll)
                       then throwError "Nothing found"
                       else
                        if (null resultDirs)
                        then return $ head resultAll
                        else throwError "Not a file"
  where
    resultAll = filter (\el -> (takeBaseName (curPath el) == name))
                                     (internal curDir)
    resultDirs = filter (\el -> (takeBaseName (curPath el) == name) && (isDirectory el))
                                     (internal curDir)

getFileOrDirectory :: Directory -> FilePath -> App Directory
getFileOrDirectory curDir name = if (null resultAll)
                                 then throwError "Nothing found"
                                 else return $ head resultAll
 where
     resultAll = filter (\el -> (takeBaseName (curPath el) == name))
                                    (internal curDir)


writeToFile :: WriteOptions -> App ()
writeToFile (WriteOptions path text) = do
  absolutePath <- getAbsolutePath path
  dir <- findDirectoryFromRoot (takeDirectory absolutePath)
  let fileName = takeBaseName path
  file <- getFile dir fileName
  let newContent = B8.pack text
  curTime <- liftIO $ getCurrentTime
  let newFile = file{content = newContent, curSize = toInteger (B8.length newContent), modificationTime = curTime}
  let newDir = dir{internal = newFile : (filter (\el -> (takeBaseName (curPath el) /= fileName)) (internal dir))}
  updateFs newDir

remove :: PathOptions -> App ()
remove (PathOptions path) = do
  absolutePath <- getAbsolutePath path
  dir <- findDirectoryFromRoot (takeDirectory absolutePath)
  let name = takeBaseName path
  let newDir = dir{internal = (filter (\el -> (takeBaseName (curPath el) /= name)) (internal dir))}
  updateFs newDir

findTheFile :: NameOptions -> App FilePath
findTheFile (NameOptions name) = do
  appState <- get
  findInto (dropExtension name) (curDirectory appState)
  `catchError` (\_ -> throwError ("File " ++ "\"" ++ name ++ "\"" ++ " is not found"))

findInto :: String -> Directory -> App FilePath
findInto name dir = do
  if (isDirectory dir)
  then do
    appState <- get
    res <- liftIO $ (runStateT (runExceptT (getFile dir name))) appState
    case res of
      (Left _, _)        -> do
        let tryAll =  map (findInto name) (internal dir)

        unpacked <- liftIO $ sequenceA $ map (\el -> ((runStateT (runExceptT (el))) appState)) tryAll
        let successful = filter (\el -> isRight (fst el)) unpacked
        if (null successful)
        then throwError ""
        else case fst (head successful) of
          Left e  -> throwError e
          Right a -> return a
      (Right fileDir, _) ->
        return $ curPath fileDir
  else throwError "dummyError"

information :: PathOptions -> App [String]
information (PathOptions path) = do
  appState <- get
  absolutePath <- getAbsolutePath path
  isRoot <-  (isRootPath absolutePath)
  if isRoot
  then do
    sizeCnt <- dirSizeFilesCnt (curDirectory appState)
    return $
      [show (fst sizeCnt), show (curDirPath appState), show (snd sizeCnt), show (curPermissions (curDirectory appState))]
  else do
    dir <- findDirectoryFromRoot (takeDirectory absolutePath)
    fileOrDir <- getFileOrDirectory dir (takeBaseName path)
    if (isDirectory fileOrDir)
    then do
      sizeCnt <- dirSizeFilesCnt fileOrDir
      return [show (fst sizeCnt), show (curPath fileOrDir), show (snd sizeCnt), show (curPermissions fileOrDir)]
    else do
      return $
        [show (curPath fileOrDir), show (curPermissions fileOrDir), show (curType fileOrDir), show (modificationTime fileOrDir), show (curSize fileOrDir)]

dirSizeFilesCnt :: Directory -> App (Integer, Int)
dirSizeFilesCnt dir = do
  if (isDirectory dir)
  then do
    cntList <- sequenceA $ map (dirSizeFilesCnt) (internal dir)
    let size = sum $ map fst cntList
    let cnt = sum $ map snd cntList
    return (size, cnt)
  else return (curSize dir, 1)

getAbsolutePath :: FilePath -> App FilePath
getAbsolutePath path =  do
  appState <- get
  if (isRelative path)
  then return $ toAbsolute (curDirectory appState) path
  else return path

createCvs :: NoOptions -> App (IO ())
createCvs _ = do
  appState <- get
  next <- createIntByDir  "../Cvs" (fileSystem appState)
  return $ sequence_ $ (createDirectoryIfMissing False "../Cvs") : (appendFile "../Cvs/history" "") : [next]

createIntByDir :: FilePath -> Directory -> App (IO ())
createIntByDir path dir = do
   let int = if (isDirectory dir)
             then internal dir
             else []
   let newPath = (path ++ "/" ++ (takeBaseName (curPath dir)))
   doAll <- sequenceA $ map (createIntByDir newPath) int
   if (isDirectory dir)
   then do
    let filePath = (newPath ++ "/isInitialized")
    let action = writeFile (newPath ++ "/isInitialized") "NO"
    fileExist <- liftIO $ doesFileExist filePath
    if fileExist
    then return $ sequence_ $ (createDirectoryIfMissing False newPath) : doAll
    else return $ sequence_ $ (createDirectoryIfMissing False newPath) : action : doAll
   else
    return $ sequence_ $ doAll

cvsAdd :: PathOptions -> App (IO ())
cvsAdd (PathOptions path) = do
  appState <- get
  absolutePath <- getAbsolutePath path
  isRoot <- (isRootPath absolutePath)
  if isRoot
  then do
    addAll $ fileSystem appState
  else do
    dir <- findDirectoryFromRoot (takeDirectory absolutePath)
    fileOrDir <- getFileOrDirectory dir (takeBaseName path)
    addAll fileOrDir
  `catch` accessHandler

addAll :: Directory -> App (IO ())
addAll curDir = do
  curDirRealPath <- fsPathToReal $ curPath curDir
  if (isDirectory curDir)
  then  do
    checkInit curDir
    doAll <- sequenceA $ map addAll (internal curDir)
    return $ sequence_ doAll
  else do
    fileExist <- liftIO $ doesFileExist $ (curDirRealPath ++ "/0/content")
    if fileExist
    then return $ return ()
    else return $ sequence_ [createDirectoryIfMissing False curDirRealPath,
                             createDirectoryIfMissing False (curDirRealPath ++ "/0"),
                             B8.writeFile (curDirRealPath ++ "/0/content")  (content curDir),
                             writeFile (curDirRealPath ++ "/lastCommitNumber")  "0",
                             writeFile (curDirRealPath ++ "/0/comment")  "init"]
  `catch` accessHandler

fsPathToReal :: FilePath -> App FilePath
fsPathToReal path = do
  appState <- get
  isRoot <- (isRootPath path)
  if isRoot
  then return $ "../Cvs/" ++ (dropExtension $ takeBaseName $ curPath $ fileSystem appState)
  else return $ "../Cvs/" ++ (dropExtension (takeBaseName (curPath (fileSystem appState)))) ++ "/"
           ++ (makeRelative (curPath (fileSystem appState)) path)

cvsInit :: NoOptions -> App (IO ())
cvsInit _ = do
  appState <- get
  let path = curDirPath appState
  absolutePath <- getAbsolutePath path
  isRoot <- (isRootPath absolutePath)
  if isRoot
  then do
   cvsInitAll $ fileSystem appState
  else do
    dir <- findDirectoryFromRoot (takeDirectory absolutePath)
    fileOrDir <- getFileOrDirectory dir (takeBaseName path)
    cvsInitAll fileOrDir
  `catch` accessHandler

cvsInitAll :: Directory -> App (IO ())
cvsInitAll curDir = do
  curDirRealPath <- fsPathToReal $ curPath curDir
  if (isDirectory curDir)
  then  do
      let filePath = (curDirRealPath ++ "/isInitialized")
      let action = writeFile filePath "YES"
      doAll <- sequenceA $ map cvsInitAll (internal curDir)
      return $ sequence_ $ action : doAll
  else return $ return ()
  `catch` accessHandler

isRootPath :: FilePath -> App Bool
isRootPath path = do
  appState <- get
  return $ equalFilePath path $ curPath $ fileSystem appState

findDirectoryFromRoot :: FilePath -> App Directory
findDirectoryFromRoot path = do
  appState <- get
  goToDirectory (fileSystem appState) path

findDirectoryHandler :: FilePath -> String -> App a
findDirectoryHandler path e = case e of
  "Not a directory" -> throwError $ path ++ " is not a directory"
  _                 -> throwError $ path ++ " does not exist"

findTheFileHandler :: FilePath -> String -> App a
findTheFileHandler path e = case e of
  "Not a file" -> throwError $ path ++ " is not a directory"
  _            -> throwError $ path ++ " does not exist"

cvsUpdate :: UpdateOptions -> App (IO ())
cvsUpdate (UpdateOptions path comment) = do
  absolutePath <- getAbsolutePath path
  isRoot <- (isRootPath absolutePath)
  if isRoot
  then do
   throwError $ path ++ " is not a folder"
  else do
    dir <- findDirectoryFromRoot (takeDirectory absolutePath)
    checkInit dir
    file <- getFile dir (takeBaseName path)
    realFilePath <- fsPathToReal $ curPath file
    exist <- liftIO $ doesDirectoryExist realFilePath
    if exist
    then do
      let lastCommitNumberFilePath = realFilePath ++ "/lastCommitNumber"
      lastCommitNumberText <- liftIO $ readFile lastCommitNumberFilePath
      let curCommitNumberText = show ((read lastCommitNumberText) + 1)
      let commitFilePath = (realFilePath ++ "/" ++ curCommitNumberText)
      return $ sequence_ [createDirectory commitFilePath,
                          writeFile (commitFilePath ++ "/comment") comment,
                          writeFile lastCommitNumberFilePath curCommitNumberText,
                          B8.writeFile (commitFilePath ++ "/content")  (content file),
                          appendFile "../Cvs/history" (path ++ ": " ++ curCommitNumberText ++ ". " ++ comment ++ "\n")
                         ]
      `catch` ochenZhalHandler
    else throwError $ path ++ " is not in CVS"
  `catch` accessHandler

checkInit :: Directory -> App ()
checkInit curDir = do
  curDirRealPath <- fsPathToReal $ curPath curDir
  initText <- liftIO $ B.readFile $ curDirRealPath ++ "/isInitialized"
  if (initText /= B8.pack "YES")
  then throwError $ "СVS is not initialized in " ++  (curPath curDir)
  else return ()
  `catch` accessHandler

cvsHistory :: PathOptions -> App (IO ())
cvsHistory (PathOptions path)= do
  absolutePath <- getAbsolutePath path
  realPath <- fsPathToReal absolutePath
  fileFolderExist <- liftIO $ doesPathExist realPath
  dir <- findDirectoryFromRoot (takeDirectory absolutePath)
  fileOrDirectory <- getFileOrDirectory dir (takeBaseName absolutePath)
  if fileFolderExist
  then
    if (isDirectory fileOrDirectory)
    then throwError $ path ++ " is not a file"
    else do
      commitsDir <- liftIO $ readDirectory realPath
      let commits = filter (isDirectory) (internal commitsDir)
      prints <- sequenceA $ map (printCommit) commits
      return $ sequence_ (reverse prints)
    `catch` ochenZhalHandler
  else throwError $ path ++ " is not in CVS"
  `catch` accessHandler

printCommit :: Directory -> App (IO ())
printCommit commit = do
  let commitNumber = takeBaseName (curPath commit)
  comment <- liftIO $ readFile ((curPath commit) ++ "/comment")
  return $ putStrLn $ commitNumber ++ ". " ++ comment

cvsCat :: VersionOptions -> App (IO ())
cvsCat (VersionOptions path commitNumber)= do
  absolutePath <- getAbsolutePath path
  realPath <- fsPathToReal absolutePath
  fileFolderExist <- liftIO $ doesPathExist realPath
  dir <- findDirectoryFromRoot (takeDirectory absolutePath)
  checkInit dir
  fileOrDirectory <- getFileOrDirectory dir (takeBaseName absolutePath)
  if fileFolderExist
  then
    if (isDirectory fileOrDirectory)
    then throwError $ path ++ " is not a file"
    else do
      commitsDir <- liftIO $ readDirectory realPath
      commitFileOrDir <- getFileOrDirectory commitsDir commitNumber
      if not $ (isDirectory commitFileOrDir)
      then throwError ""
      else do
        commitContent <- liftIO $ B8.readFile $ (curPath commitFileOrDir) ++ "/content"
        return $ B8.putStrLn $ commitContent
      `catchError` commitHandler
  else throwError $ path ++ " is not in CVS"
  `catch` accessHandler

cvsMerge :: MergeOptions -> App ()
cvsMerge (MergeOptions path commitNum1 commitNum2 option) = do
  absolutePath <- getAbsolutePath path
  isRoot <- (isRootPath absolutePath)
  let error1 = commitNum1 ++ "doesn't exist"
  let error2 = commitNum2 ++ "doesn't exist"
  let errorFile = path ++ "is not added to CVS"
  if isRoot
  then do
   throwError $ path ++ " is not a folder"
  else do
    dir <- findDirectoryFromRoot (takeDirectory absolutePath)
    checkInit dir
    file <- getFile dir (takeBaseName path)
    realFilePath <- fsPathToReal $ curPath file
    fileFolderExist <- liftIO $ doesPathExist realFilePath
    if fileFolderExist
    then do
      let commit1FilePath = realFilePath ++ "/" ++ commitNum1 ++ "/content"
      --throwError commit1FilePath
      exist1 <-  liftIO $ doesFileExist commit1FilePath
      if exist1
      then do
        commit1Text <- liftIO $ readFile commit1FilePath
        let commit2FilePath = realFilePath ++ "/" ++ commitNum2 ++ "/content"
        exist2 <-  liftIO $ doesFileExist commit2FilePath
        if exist2
        then do
          commit2Text <- liftIO $ readFile commit2FilePath
          case option of
            "Left" -> writeToFile $ WriteOptions path $ commit1Text
            "Right" -> writeToFile $ WriteOptions path $ commit2Text
            "Both" -> writeToFile $ WriteOptions path $ commit1Text ++ "\n" ++ ">>>" ++ "\n" ++ commit2Text
        else throwError $ error2
      else throwError $ error1
    else throwError $ errorFile
  `catch` accessHandler

commitHandler :: String -> App a
commitHandler _ = throwError "Commit doesn't exist"

cvsRemove :: PathOptions -> App (IO ())
cvsRemove (PathOptions path) = do
  absolutePath <- getAbsolutePath path
  isRoot <- (isRootPath absolutePath)
  let errorFile = path ++ "is not added to CVS"
  if isRoot
  then do
   throwError $ path ++ " is not a folder"
  else do
    dir <- findDirectoryFromRoot (takeDirectory absolutePath)
    checkInit dir
    file <- getFile dir (takeBaseName path)
    realFilePath <- fsPathToReal $ curPath file
    fileFolderExist <- liftIO $ doesPathExist realFilePath
    if fileFolderExist
    then do
      return $ removeDirectoryRecursive realFilePath
      `catch` ochenZhalHandler
    else throwError $ errorFile
  `catch` accessHandler

cvsRemoveVersion :: VersionOptions -> App (IO ())
cvsRemoveVersion (VersionOptions path commitNum) = do
  absolutePath <- getAbsolutePath path
  isRoot <- (isRootPath absolutePath)
  let errorFile = path ++ "is not added to CVS"
  if isRoot
  then do
   throwError $ path ++ " is not a folder"
  else do
    dir <- findDirectoryFromRoot (takeDirectory absolutePath)
    checkInit dir
    file <- getFile dir (takeBaseName path)
    realFilePath <- fsPathToReal $ curPath file
    fileFolderExist <- liftIO $ doesPathExist realFilePath
    if fileFolderExist
    then do
      let commitFilePath = realFilePath ++ "/" ++ commitNum
      exist <-  liftIO $ doesDirectoryExist commitFilePath
      if exist
      then do
        return $ removeDirectoryRecursive commitFilePath
        `catch` ochenZhalHandler
      else throwError "Commit doesn't exist"
    else throwError $ errorFile
  `catch` accessHandler

cvsHistoryAll :: NoOptions -> App (IO ())
cvsHistoryAll _ = do
  let path = "../Cvs/history"
  exist <- liftIO $ doesFileExist path
  if exist
  then do
    history <- liftIO $ readFile $ path
    return $ putStrLn history
  else throwError "History record is not found"
  `catch` accessHandler

showHelp :: NoOptions -> App (IO ())
showHelp _ = do
  let path = "../help"
  exist <- liftIO $ doesFileExist path
  if exist
  then do
    help <- liftIO $ readFile $ path
    return $ putStrLn help
  else throwError "Help file is not found"
  `catch` accessHandler

rewriteFs :: NoOptions -> App (IO ())
rewriteFs _ = do
  appState <- get
  let path = curPath $ fileSystem appState
  let action1 = removeDirectoryRecursive path
  action2 <- writeFs (fileSystem appState)
  return $ sequence_ $ [action1, action2]
  `catch` accessHandler

writeFs :: Directory -> App (IO ())
writeFs dir = do
  let path = curPath dir
  if isDirectory dir
  then do
    let action = createDirectory path
    doAll <- sequenceA $ map writeFs (internal dir)
    return $ sequence_ $ action : doAll
  else do
    return $ B8.writeFile path (content dir)
  `catch` accessHandler

ochenZhalHandler :: CE.IOException -> App a
ochenZhalHandler _ = throwError "CVS files are damaged. Ochen zhal vas"

accessHandler :: CE.IOException -> App a
accessHandler err = do
   throwError $ "Access issues: " ++ show err