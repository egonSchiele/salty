module Lib where

import Types
import Text.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.Combinator
--import Text.ParserCombinators.Parsec.Error(messageString)
import Data.List (intercalate)

build :: String -> String
build str = case (parse saltyParser "saltyParser" str) of
                 Left err -> show err
                 Right xs -> xs

-- if you don't use try, and the first parser consumes some input,
-- parser #2 doesn't use that input
(<||>) p1 p2 = try(p1) <|> p2


saltyParser = do
       instanceVar
  <||> classVar
  <||> method
  <||> invertedIf
  <||> (many1 anyToken)

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


-- build a b := 2
method = do
  functionName <- many1 letter
  space
  parameters <- many1 methodParameter
  string ":="
  spaces
  body_ <- (singleLineMethodBody <||> multiLineMethodBody)
  let body = build body_
  return $ "function " ++ functionName ++ "(" ++ (intercalate ", " (map (\n -> '$':n) parameters)) ++ ") {\n" ++ body ++ "\n}"

singleLineMethodBody = do
  skipMany $ string "return" >> spaces
  body <- many1 $ noneOf "\r\n"
  return $ "return " ++ body ++ ";"

multiLineMethodBody = anyToken `manyTill` eof

methodParameter = do
  name <- many1 letter
  space
  return name

invertedIf = do
  action <- manyTill anyToken (lookAhead (string "if"))
  string "if"
  space
  condition <- manyTill anyToken eof
  return $ "if (" ++ condition ++ ") {\n" ++ action ++ "\n}"
