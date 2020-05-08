module ArgumentsParser where

import Options.Applicative
import Control.Monad.Except
import System.Environment

run :: [String] -> ParserInfo op -> String -> IO (Either String op)
run args curOpts name = do
  opts <- execParserWithHelp args (prefs idm) curOpts name
  return opts

execParserWithHelp :: [String] -> ParserPrefs -> ParserInfo a -> String -> IO (Either String a)
execParserWithHelp args pprefs pinfo name = handleParseResult' (execParserPure pprefs pinfo args) name

handleParseResult' :: ParserResult a -> String -> IO (Either String a)
handleParseResult' (Success a) _ = return (Right a)
handleParseResult' (Failure failure) name = do
      let (msg, _) = renderFailure failure name
      return (Left msg)
handleParseResult (CompletionInvoked compl) name = do
      msg <- execCompletion compl name
      return (Left msg)