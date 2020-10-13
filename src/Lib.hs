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
getRight (Left x) = SaltyString $ "error adit: " ++ (show x)

parse_ :: String -> Salty
parse_ str = getRight $ parse saltyParser "saltyParser" str

saltyParser = do
  function
  <||> assignment
  <||> saltyString
  <||> saltyNumber
  <||> returnStatement
  <||> higherOrderFunctionCall
  <||> phpLine

variableName = do
        classVar
  <||>  instanceVar
  <||>  simpleVar

functionBody = do
        block
  <||>  lambda
  <||>  ampersand
  <||>  oneLine

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
  lookAhead (oneOf " .),")
  return $ InstanceVar variable

-- @@foo
classVar = do
  string "@@"
  variable <- many1 letter
  lookAhead (oneOf " .),")
  return $ ClassVar variable

-- foo
simpleVar = do
  variable <- many1 letter
  lookAhead (oneOf " .),")
  return $ SimpleVar variable

oneLine = do
  parserTrace "oneline1"
  line <- anyChar `manyTill` (lookAhead $ char ')')
  parserTrace $ "oneline2: " ++ line
  let salty = parse_ line
  parserTrace "oneline3"
  return $ OneLine salty

block = do
  string "do"
  newline
  blockLines <- anyToken `manyTill` (string "end")
  let l = lines blockLines
  return $ Block (map parse_ l)

lambda = do
  string "\\"
  parserTrace "lamb1"
  args <- anyToken `manyTill` (string "->")
  spaces
  parserTrace "lamb2"
  body <- functionBody
  parserTrace $ "lamb3: " ++ (show body)
  return $ LambdaFunction (words args) body

ampersand = do
  char '&'
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

eachFunc = string ".each" >> return Each
mapFunc = string ".map" >> return Map
selectFunc = string ".select" >> return Select
anyFunc = string ".any" >> return Any
allFunc = string ".all" >> return All

higherOrderFunction = do
       eachFunc
  <||> mapFunc
  <||> selectFunc
  <||> anyFunc
  <||> allFunc

higherOrderFunctionCall = do
  parserTrace "1"
  obj <- variableName
  parserTrace "2"
  hof <- higherOrderFunction
  parserTrace "3"
  char '('
  parserTrace "4"
  func <- functionBody
  parserTrace "5"
  char ')'
  return $ HigherOrderFunctionCall obj hof func

phpLine = do
  line <- many1 anyChar
  return $ PhpLine line

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
