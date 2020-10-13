module Lib where

import Types
import Text.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.Combinator
--import Text.ParserCombinators.Parsec.Error(messageString)
import Data.List (intercalate)
import qualified Data.Functor.Identity

for = flip map

saltyToPhp :: String -> String
saltyToPhp str = case (build str) of
                   Left err -> show err
                   Right xs -> saltyToPhp_ xs

saltyToPhp_ :: [Salty] -> String
saltyToPhp_ tree = unlines . indent . addSemicolons . lines . concat . (map toPhp) $ tree

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
  where newLine = if (last l) == '}'
                     then (replicate ((indentAmt-1)*4) ' ') ++ l
                     else (replicate (indentAmt*4) ' ') ++ l
        newAmt = case (last l) of
                      '{' -> indentAmt + 1
                      '}' -> indentAmt - 1
                      _ -> indentAmt


build :: String -> Either ParseError [Salty]
build str = parse saltyParser "saltyParser" (str ++ "\n")

-- if you don't use try, and the first parser consumes some input,
-- parser #2 doesn't use that input
(<||>) p1 p2 = try(p1) <|> p2

getRight (Right x) = x
getRight (Left x) = SaltyString $ "error adit: " ++ (show x)

parse_ :: String -> Salty
parse_ str = getRight $ parse saltyParserSingle "saltyParserSingle" str

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
  body <- (block <||> oneLine)
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
  line <- anyChar `manyTill` (lookAhead $ oneOf ")\n")
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
  body_ <- anyToken `manyTill` (lookAhead $ oneOf ")\n")
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
  func <- (lambda <||> ampersand)
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
