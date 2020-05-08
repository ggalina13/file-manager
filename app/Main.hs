{-# LANGUAGE FlexibleContexts #-}
module Main where

import FileSystem
import ArgumentsParser
import System.IO
import Commands
import Control.Monad.State
import Control.Monad.Except
import Options.Applicative
import ParserOptions
import Data.Void
import qualified  Data.ByteString.Char8 as B8
import InputParser
import qualified Text.Megaparsec as TM
import qualified Data.Text as DT
import Control.Exception
import System.Directory

main :: IO ()
main = do
  putStrLn $ "Please, enter the directory starting from which the file-manager will have access or enter \"exit\""
  input <- getLine
  case input of
   "exit" -> return ()
   _      -> do
    exist <- doesDirectoryExist input
    case exist of
      False -> do
        putStrLn $ "Incorrect path"
        main
      True  -> do
        fs <- readDirectory input
        let appState = AppState fs fs
        runCommandWithOptions True ["cmd"] noOpts appState createCvs id
   `catch` dirAccessHandler

dirAccessHandler :: IOException -> IO ()
dirAccessHandler err = do
  putStrLn $ "Directory access issues: " ++ show err
  return ()

continue :: AppState -> IO ()
continue appState = do
  hFlush stdout
  putStr $ (curPath (curDirectory appState)) ++ "/ > "
  hFlush stdout
  input <- getLine
  if (input == "")
  then continue appState
  else do
    runCommand appState input

runCommand :: AppState -> String -> IO ()
runCommand appState input = do
  parseRes <- parseParams input
  case parseRes of
    Left _       -> do
      putStrLn $ "Wrong command. Try help"
      continue appState
    Right params -> do
      case (head params) of
        "cd"                      -> runCommandWithOptions True params pathOpts appState cd doNothingHandler
        "ls"                      -> runCommandWithOptions True params pathOpts appState ls printDir
        "dir"                     -> runCommandWithOptions True params noOpts appState getDir printDir
        "create-folder"           -> runCommandWithOptions True params nameOpts appState createFolder doNothingHandler
        "cat"                     -> runCommandWithOptions True params pathOpts appState cat (B8.putStrLn)
        "write-file"              -> runCommandWithOptions True params writeOpts appState writeToFile doNothingHandler
        "create-file"             -> runCommandWithOptions True params nameOpts appState createFile doNothingHandler
        "remove"                  -> runCommandWithOptions True params pathOpts appState remove doNothingHandler
        "find-file"               -> runCommandWithOptions True params nameOpts appState findTheFile putStrLn
        "information"             -> runCommandWithOptions True params pathOpts appState information printDir
        "cvs-add"                 -> runCommandWithOptions True params pathOpts appState cvsAdd id
        "cvs-init"                -> runCommandWithOptions True params noOpts appState cvsInit id
        "cvs-update"              -> runCommandWithOptions True params updateOpts appState cvsUpdate id
        "cvs-history"             -> runCommandWithOptions True params pathOpts appState cvsHistory id
        "cvs-cat"                 -> runCommandWithOptions True params versionOpts appState cvsCat id
        "cvs-merge-revs"          -> runCommandWithOptions True params mergeOpts appState cvsMerge doNothingHandler
        "cvs-delete-version"      -> runCommandWithOptions True params versionOpts appState cvsRemoveVersion id
        "cvs-remove"              -> runCommandWithOptions True params pathOpts appState cvsRemove id
        "cvs-show-everything"     -> runCommandWithOptions True params noOpts appState cvsHistoryAll id
        "help"                    -> runCommandWithOptions True params noOpts appState showHelp id
        "sync"                    -> runCommandWithOptions True params noOpts appState rewriteFs id
        "exit"                    -> runCommandWithOptions False params noOpts appState rewriteFs id
        _                         -> do
          putStrLn $ "Wrong command. Try help"
          continue appState

runCommandWithOptions :: Bool ->
                         [String] ->
                         ParserInfo a ->
                         AppState ->
                         (a -> App b) ->
                         (b -> IO ()) ->
                         IO ()
runCommandWithOptions contin params opts appState f handler = do
  let name = head params
  errorOrOptions <- run (tail params) opts name
  case errorOrOptions of
    Left e        -> do
      putStrLn e
      continue appState
    Right options -> do
      res <- (runStateT (runExceptT (f options)) appState)
      case res of
        (Left e1, newState1)  -> do
          putStrLn e1
          if contin
          then continue newState1
          else return ()
        (Right ans, newState2) -> do
          handler ans
          if contin
          then continue newState2
          else return ()

doNothingHandler :: a -> IO ()
doNothingHandler _ = return ()

parseParams :: String -> IO (Either (TM.ParseErrorBundle DT.Text Data.Void.Void) [String])
parseParams input = do
  return $ TM.runParser argsBegin "" $ DT.pack input

printDir :: [String] -> IO ()
printDir []       = putStrLn "Folder is empty"
printDir (x : []) = putStrLn x
printDir (x : xs) = do
  putStrLn x
  printDir xs