module InputParser where

import Data.Void
import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text as DT
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void DT.Text

escapedSymbols :: [Char]
escapedSymbols = ['\\', '\"', '\0', '\n', '\r', '\v', '\t', '\b', '\f']

escapedParts :: [Char]
escapedParts = ['\\', '\"', '0', 'n', 'r', 'v', 't', 'b', 'f']

escape :: Parser String
escape = do
  char '\\'
  c <- oneOf escapedParts
  return $ '\\' : [c]

notEscaped:: Parser Char
notEscaped = noneOf escapedSymbols

quotedArgNoQuotes :: Parser String
quotedArgNoQuotes = do
  char '"'
  parts <- many (fmap return (try notEscaped) <|> escape)
  char '"'
  return $ concat parts

quotedArg :: Parser String
quotedArg = do
  res <- quotedArgNoQuotes
  return $ "\"" ++ res ++ "\""

argsBegin :: Parser [String]
argsBegin = do
  first <- word
  next <- (char ' ' >> args) <|> (return [])
  return (first : next)

args :: Parser [String]
args = do
  first <- (try quotedArg) <|> word
  next <- (char ' ' >> args) <|> (return [])
  return (first : next)

word :: Parser String
word = do
  letter <- satisfy (notSpace)
  res <- many (satisfy (notSpace))
  return $ (letter : res)

notSpace :: (Char -> Bool)
notSpace ' ' = False
notSpace _   = True

quotedNumber :: Parser String
quotedNumber = do
  char '"'
  number <- L.decimal
  char '"'
  return $ show number