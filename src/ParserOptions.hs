{-# LANGUAGE FlexibleContexts #-}
module ParserOptions where

import Options.Applicative
import Control.Monad.Except
import System.Environment
import ArgumentsParser
import InputParser
import qualified Text.Megaparsec as TM
import qualified Data.Text as DT

data PathOptions = PathOptions { pathOption :: FilePath } deriving Show
data NameOptions = NameOptions { nameOption :: String } deriving Show
data WriteOptions = WriteOptions { filePathOption :: String,
                                   textOption :: String
                                 } deriving Show
data UpdateOptions = UpdateOptions { updateFilePathOption :: String,
                                     commentOption :: String
                                   } deriving Show
data VersionOptions = VersionOptions { commitedFilePath :: String,
                                       commitNumber :: String
                                     } deriving Show
data MergeOptions = MergeOptions { mergeFilePath :: String,
                                   commitNumber1 :: String,
                                   commitNumber2 :: String,
                                   mergeOption :: String
                                 } deriving Show
data NoOptions = NoOptions deriving Show

pathOpts :: ParserInfo PathOptions
pathOpts = info parser mempty
  where
    parser = PathOptions <$> argument str (metavar "PATH")

nameOpts :: ParserInfo NameOptions
nameOpts = info parser mempty
  where
    parser = NameOptions <$> argument quote (metavar "\"NAME\"")

writeOpts :: ParserInfo WriteOptions
writeOpts = info parser mempty
  where
    parser = WriteOptions <$> argument str (metavar "PATH")
                          <*> argument quote (metavar "\"TEXT\"")

updateOpts :: ParserInfo UpdateOptions
updateOpts = info parser mempty
  where
    parser = UpdateOptions <$> argument str (metavar "PATH")
                           <*> argument quote (metavar "\"COMMENT\"")

versionOpts :: ParserInfo VersionOptions
versionOpts = info parser mempty
  where
    parser = VersionOptions <$> argument str (metavar "PATH")
                            <*> argument quotedNumberNoQuotes (metavar "\"COMMIT_NUMBER\"")

mergeOpts :: ParserInfo MergeOptions
mergeOpts = info parser mempty
  where
    parser = MergeOptions <$> argument str (metavar "PATH")
                          <*> argument quotedNumberNoQuotes (metavar "\"COMMIT_NUMBER1\"")
                          <*> argument quotedNumberNoQuotes (metavar "\"COMMIT_NUMBER2\"")
                          <*> argument mergeVariant (metavar "\"Left\" | \"Right\" | \"Both\"")

noOpts :: ParserInfo NoOptions
noOpts = info parser mempty
  where
   parser = pure NoOptions

quote :: ReadM String
quote = eitherReader f
  where
    f s = case (TM.runParser quotedArgNoQuotes "" $ DT.pack s) of
      Right a -> return $ a
      Left _  -> throwError $ "Invalid argument `" ++ s ++ "`: quotation marks expected"

quotedNumberNoQuotes :: ReadM String
quotedNumberNoQuotes = eitherReader f
  where
    f s = case (TM.runParser quotedNumber "" $ DT.pack s) of
      Right a -> return $ a
      Left _  -> throwError $ "Invalid argument `" ++ s ++ "`: number in quotation marks expected"

mergeVariant :: ReadM String
mergeVariant = eitherReader f
  where
    f s = case s of
      "\"Left\""  -> return "Left"
      "\"Right\"" -> return "Right"
      "\"Both\""  -> return "Both"
      _ -> throwError $ "Invalid argument `" ++ s ++ "`: Left | Right | Both"