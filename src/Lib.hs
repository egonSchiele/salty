module Lib where

import Text.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.Combinator
--import Text.ParserCombinators.Parsec.Error(messageString)


build :: String -> String
build str = case (parse saltyParser "saltyParser" str) of
                 Left err -> show err
                 Right xs -> xs

-- if you don't use try, and the first parser consumes some input,
-- parser #2 doesn't use that input
(<||>) p1 p2 = try(p1) <|> p2


saltyParser = instanceVar <||> classVar

-- @foo = 1
instanceVar = do
  char '@'
  variable <- many1 letter
  spaces
  char '='
  spaces
  value <- anyToken `manyTill` eof
  return $ "$this->" ++ variable ++ " = " ++ value

-- @@foo = 1
classVar = do
  string "@@"
  variable <- many1 letter
  spaces
  char '='
  spaces
  value <- anyToken `manyTill` eof
  return $ "self::$" ++ variable ++ " = " ++ value
