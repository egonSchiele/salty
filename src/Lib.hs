module Lib where

import Types
import Text.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.Combinator
--import Text.ParserCombinators.Parsec.Error(messageString)
import Data.List (intercalate)

build :: String -> String
build str = case (parse saltyParser "saltyParser" (str ++ "\n")) of
                 Left err -> show err
                 Right xs -> toPhp xs

-- if you don't use try, and the first parser consumes some input,
-- parser #2 doesn't use that input
(<||>) p1 p2 = try(p1) <|> p2

getRight (Right x) = x
getRight (Left x) = SaltyString $ "error: " ++ (show x)

parse_ :: String -> Salty
parse_ str = getRight $ parse saltyParser "saltyParser" str

saltyParser = do
  function
  <||> assignment
  <||> saltyString
  <||> saltyNumber
  <||> returnStatement

variableName = do
        classVar
  <||>  instanceVar
  <||>  simpleVar

functionBody = do
        oneLine
  <||>  block
  <||>  lambda
  <||>  ampersand

function = do
  name <- variableName
  args <- anyToken `manyTill` (string ":=")
  spaces
  body <- functionBody
  return $ Function name (map argWithDefaults (words args)) body

assignmentType = do
       (string "=" >> return Equals)
  <||> (string "+=" >> return PlusEquals)
  <||> (string "-=" >> return MinusEquals)
  <||> (string "||=" >> return OrEquals)

assignment = do
  name <- variableName
  spaces
  typ <- assignmentType
  spaces
  value <- saltyParser
  return $ Assignment name typ value

betweenQuotes = between (oneOf "\"'") (oneOf "\"'")

saltyString = do
  str <- betweenQuotes (many anyToken)
  return $ SaltyString str

saltyNumber = do
  number <- many1 (oneOf "1234567890.")
  return $ SaltyNumber number

-- @foo
instanceVar = do
  char '@'
  variable <- many1 letter
  try space
  return $ InstanceVar variable

-- @@foo
classVar = do
  string "@@"
  variable <- many1 letter
  try space
  return $ ClassVar variable

-- foo
simpleVar = do
  variable <- many1 letter
  try space
  return $ SimpleVar variable

oneLine = do
  line <- many1 anyChar
  let salty = parse_ line
  return $ OneLine salty

block = do
  string "do"
  newline
  blockLines <- anyToken `manyTill` (string "end")
  let l = lines blockLines
  return $ Block (map parse_ l)

lambda = do
  string "\\"
  args <- (many1 letter) `sepBy` space
  body <- functionBody
  return $ LambdaFunction args body

ampersand = do
  string "&"
  var <- variableName
  return $ AmpersandFunction var

returnStatement = do
  string "return "
  line <- anyToken `manyTill` (try newline)
  let salty = parse_ line
  return $ ReturnStatement salty

hashLookup = do
  h <- variableName
  space
  char '>'
  space
  k <- variableName
  return $ HashLookup (Left h) k

-- build a b := 2
-- function = do
--   functionName <- many1 letter
--   space
--   parameters <- many1 functionParameter
--   string ":="
--   spaces
--   body_ <- (singleLinefunctionBody <||> multiLineFunctionBody)
--   let body = build body_
--   return $ "function " ++ functionName ++ "(" ++ (intercalate ", " (map (\n -> '$':n) parameters)) ++ ") {\n" ++ body ++ "\n}"

-- singleLineFunctionBody = do
--   skipMany $ string "return" >> spaces
--   body <- many1 $ noneOf "\r\n"
--   return $ "return " ++ body ++ ";"

-- multiLineFunctionBody = anyToken `manyTill` eof

-- functionParameter = do
--   name <- many1 letter
--   space
--   return name

-- invertedIf = do
--   action <- manyTill anyToken (lookAhead (string "if"))
--   string "if"
--   space
--   condition <- manyTill anyToken eof
--   return $ "if (" ++ condition ++ ") {\n" ++ action ++ "\n}"
