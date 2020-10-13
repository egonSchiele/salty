module Lib where

import Types
import Text.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.Combinator
--import Text.ParserCombinators.Parsec.Error(messageString)
import Data.List (intercalate)
import qualified Data.Functor.Identity
import Utils

for = flip map

saltyToPhp :: String -> String
saltyToPhp str = case (build str) of
                   Left err -> show err
                   Right xs -> saltyToPhp_ xs

saltyToDebugTree :: String -> String
saltyToDebugTree str = case (build str) of
                   Left err -> show err
                   Right xs -> formatDebug (show xs)

saltyToPhp_ :: [Salty] -> String
saltyToPhp_ tree = unlines . indent . addSemicolons . lines . (intercalate "\n") . (map toPhp) $ tree

formatDebug :: String -> String
formatDebug str = unlines . indentDebug . (map strip) . lines . addNewlines $ str

addNewlines str = replace "[" "[\n" . replace "{" "{\n" . replace "," ",\n" . replace "}" "\n}" . replace "]" "\n]" $ str

addSemicolons :: [String] -> [String]
addSemicolons phpLines = for phpLines $ \line ->
                              if (line == "") || (last line) `elem` ['{', '}', ';']
                                 then line
                                 else (line ++ ";")

indent :: [String] -> [String]
indent lines_ = indent_ lines_ 0

indent_ :: [String] -> Int -> [String]
indent_ [] _ = []
indent_ ("":lines_) indentAmt = "":(indent_ lines_ indentAmt)
indent_ (l:lines_) indentAmt = newLine:(indent_ lines_ newAmt)
  where newLine = if (last l) == '}' || (last_ 2 l) == "};"
                     then (replicate ((indentAmt-1)*4) ' ') ++ l
                     else (replicate (indentAmt*4) ' ') ++ l
        newAmt = newAmt_ l indentAmt


newAmt_ l indentAmt
  | (last l) == '{' = indentAmt + 1
  | (last l) == '}' = indentAmt - 1
  | (last_ 2 l) == "};" = indentAmt - 1
  | otherwise = indentAmt


indentDebug :: [String] -> [String]
indentDebug lines_ = indentDebug_ lines_ 0

indentDebug_ :: [String] -> Int -> [String]
indentDebug_ [] _ = []
indentDebug_ ("":lines_) indentAmt = "":(indentDebug_ lines_ indentAmt)
indentDebug_ (l:lines_) indentAmt = newLine:(indentDebug_ lines_ newAmt)
  where newLine = if (last l) `elem` ['}', ']'] || (last_ 2 l) `elem` ["},", "],"]
                     then (replicate ((indentAmt-1)*4) ' ') ++ l
                     else (replicate (indentAmt*4) ' ') ++ l
        newAmt = getNewAmt l indentAmt

getNewAmt l indentAmt
  | (last l) `elem` ['}', ']'] = indentAmt - 1
  | (last_ 2 l) `elem` ["},", "],"] = indentAmt - 1
  | (last l) `elem` ['{', '['] = indentAmt + 1
  | otherwise = indentAmt

build :: String -> Either ParseError [Salty]
build str = parse saltyParser "saltyParser" (str ++ "\n")

-- if you don't use try, and the first parser consumes some input,
-- parser #2 doesn't use that input
(<||>) p1 p2 = try(p1) <|> p2

getRight (Right x) = x
getRight (Left x) = SaltyString $ "error adit: " ++ (show x)

parse_ :: String -> Salty
parse_ "" = PhpLine ""
parse_ str = getRight $ parse saltyParserSingle "saltyParserSingle" newStr
  where newStr = if (last str == '\n') then str else (str ++ "\n")

saltyParser :: ParsecT String u Data.Functor.Identity.Identity [Salty]
saltyParser = many saltyParserSingle

saltyParserSingle :: ParsecT String u Data.Functor.Identity.Identity Salty
saltyParserSingle = do
  function
  <||> assignment
  <||> saltyString
  <||> saltyNumber
  <||> returnStatement
  <||> higherOrderFunctionCall
  <||> functionCall
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
  body_ <- anyChar `manyTill` char ';'
  let body = parse_ body_
  return $ Function name (map argWithDefaults (words args)) (OneLine body)

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
  value <- saltyParserSingle
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
  variable <- letter `manyTill` (lookAhead (oneOf " .),"))
  return $ InstanceVar variable

-- @@foo
classVar = do
  string "@@"
  variable <- letter `manyTill` (lookAhead (oneOf " .),"))
  return $ ClassVar variable

-- foo
simpleVar = do
  variable <- letter `manyTill` (lookAhead (oneOf " .),"))
  return $ SimpleVar variable

oneLine = do
  line <- anyChar `manyTill` (try $ char ';')
  parserTrace $ ">> " ++ line ++ "<<"
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
  args <- anyToken `manyTill` (string "->")
  spaces
  body_ <- anyToken `manyTill` (lookAhead $ char '\n')
  let body = parse_ body_
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
  obj <- variableName
  hof <- higherOrderFunction
  char '('

  string "\\"
  args <- anyToken `manyTill` (try $ string "->")
  spaces
  body_ <- functionCall
  let func = LambdaFunction (words args) body_
  return $ HigherOrderFunctionCall obj hof func

phpLine = do
  line <- anyChar `manyTill` (string "\n")
  return $ PhpLine line

functionCall = do
  obj <- variableName
  char '.'
  funcName <- letter `manyTill` (lookAhead $ char '(')
  funcArgs <- anyChar `manyTill` (lookAhead . try $ char ')')
  return $ FunctionCall (Just obj) (SimpleVar funcName) (split "," funcArgs)

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
