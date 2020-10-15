module Lib where

import Types
import Text.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.Combinator
--import Text.ParserCombinators.Parsec.Error(messageString)
import Utils
import Formatting
import System.IO.Unsafe
import System.Environment
import Debug.Trace (trace)

type SaltyState = Salty
type SaltyParser = Parsec String SaltyState Salty

debug :: String -> SaltyParser
-- debug str = return (SaltyString str)
debug str = parserTrace str >> return (SaltyString str)

saltyToPhp :: String -> String
saltyToPhp str = case (build str) of
                   Left err -> show err
                   Right xs -> saltyToPhp_ xs

saltyToDebugTree :: String -> String
saltyToDebugTree str = case (build str) of
                   Left err -> show err
                   Right xs -> formatDebug xs

build :: String -> Either ParseError [Salty]
build str = runParser saltyParser EmptyLine "saltyParser" (str ++ "\n")

saltyParser :: Parsec String SaltyState [Salty]
saltyParser = do
  parserTrace "start"
  many saltyParserSingle

saltyParserSingle :: SaltyParser
saltyParserSingle = debug "saltyParserSingle" >> do
  salty <- saltyParserSingle_
  optional $ char '\n'
  return salty

saltyParserSingle_ :: SaltyParser
saltyParserSingle_ = do
  debug "saltyParserSingle_"
  salty <- saltyParserSingle__
  putState salty
  return salty

saltyParserSingle__ :: SaltyParser
saltyParserSingle__ = do
  parens
  <||> function
  <||> operation
  <||> partialOperation
  <||> saltyString
  <||> saltyNumber
  <||> returnStatement
  <||> higherOrderFunctionCall
  <||> functionCall
  <||> negateSalty
  <||> saltyComment
  <||> phpComment
  <||> phpLine
  <||> emptyLine

variableName = debug "variableName" >> do
        classVar
  <||>  instanceVar
  <||>  simpleVar

parens = debug "parens" >> do
  char '('
  body <- saltyParserSingle
  char ')'
  return $ Parens body

parensWith :: SaltyParser -> SaltyParser
parensWith parser = debug "parensWith" >> do
  char '('
  body <- parser
  char ')'
  return $ Parens body

function = debug "function" >> do
  name <- variableName
  parserTrace "function-aa"
  space
  parserTrace "function-bb"
  args <- many1 (letter <|> digit <|> space <|> (char '_'))
  string ":="
  space
  parserTrace "function-cc"
  body <- saltyParserSingle
  parserTrace "function-dd"
  return $ Function name (map argWithDefaults (words args)) body

operator = debug "operator" >> do
       (string "=" >> return Equals)
  <||> (string "!=" >> return NotEquals)
  <||> (string "+=" >> return PlusEquals)
  <||> (string "-=" >> return MinusEquals)
  <||> (string "/=" >> return DivideEquals)
  <||> (string "*=" >> return MultiplyEquals)
  <||> (string "||=" >> return OrEquals)
  <||> (string "||" >> return OrOr)
  <||> (string "&&" >> return AndAnd)
  <||> (string "<" >> return LessThan)
  <||> (string "<=" >> return LessThanOrEqualTo)
  <||> (string ">" >> return GreaterThan)
  <||> (string ">=" >> return GreaterThanOrEqualTo)
  <||> (string "+" >> return Add)
  <||> (string "-" >> return Subtract)
  <||> (string "/" >> return Divide)
  <||> (string "*" >> return Multiply)

atom = debug "atom" >> do
       (Left <$> variableName)
  <||> (Right <$> saltyString)
  <||> (Right <$> saltyNumber)

operation = debug "operation" >> do
  left <- atom
  space
  op <- operator
  space
  right <- ((Right <$> operation) <||> atom)
  return $ Operation left op right

partialOperation = debug "partialOperation" >> do
  space
  op <- operator
  space
  right <- ((Right <$> operation) <||> atom)
  leftHandSide <- getState
  return $ BackTrack (Operation (Right leftHandSide) op right)

negateSalty = debug "negateSalty" >> do
  char '!'
  s <- saltyParserSingle
  return $ Negate s

emptyLine = debug "emptyLine" >> do
  string "\n"
  return EmptyLine

saltyString = debug "saltyString" >> do
  oneOf "'\""
  str <- many $ noneOf "\"'"
  oneOf "'\""
  return $ SaltyString str

saltyNumber = debug "saltyNumber" >> do
  number <- many1 (oneOf "1234567890.")
  return $ SaltyNumber number

-- @foo
instanceVar = debug "instanceVar" >> do
  char '@'
  variable <- many1 letter
  lookAhead $ oneOf endDelim
  return $ InstanceVar variable

-- @@foo
classVar = debug "classVar" >> do
  string "@@"
  variable <- many1 letter
  lookAhead $ oneOf endDelim
  return $ ClassVar variable

-- foo
simpleVar = debug "simpleVar" >> do
  variable <- many1 letter
  lookAhead $ oneOf endDelim
  return $ SimpleVar variable

-- block = do
--   string "do"
--   newline
--   blockLines <- anyToken `manyTill` (string "end")
--   let l = lines blockLines
--   return $ Block (map parse_ l)

lambda = debug "lambda" >> do
  string "\\"
  args <- anyToken `manyTill` (string " -> ")
  body <- saltyParserSingle
  return $ LambdaFunction (words args) body

-- ampersand = debug "ampersand" >> do
--   char '&'
--   var <- variableName
--   return $ AmpersandFunction var

returnStatement = debug "returnStatement" >> do
  string "return "
  salty <- saltyParserSingle
  return $ ReturnStatement salty

eachFunc = string ".each" >> return Each
mapFunc = string ".map" >> return Map
selectFunc = string ".select" >> return Select
anyFunc = string ".any" >> return Any
allFunc = string ".all" >> return All

higherOrderFunction = debug "higherOrderFunction" >> do
       eachFunc
  <||> mapFunc
  <||> selectFunc
  <||> anyFunc
  <||> allFunc

higherOrderFunctionCall = debug "higherOrderFunctionCall" >> do
  obj <- variableName
  hof <- higherOrderFunction
  (Parens func) <- parensWith lambda
  return $ HigherOrderFunctionCall obj hof func

saltyComment = do
  char '#'
  line <- anyChar `manyTill` (string "\n")
  return $ SaltyComment line

phpComment = do
  string "// "
  line <- anyChar `manyTill` (string "\n")
  return $ PhpComment line

phpLine = do
  char '-'
  line <- anyChar `manyTill` (string "\n")
  return $ PhpLine line

functionCall = debug "functionCall" >> do
       functionCallOnObject
  <||> functionCallWithoutObject

functionCallOnObject = debug "functionCallOnObject" >> do
       functionCallWithArgs
  <||> functionCallWithoutArgs

functionCallWithArgs = debug "functionCallWithArgs" >> do
  obj <- variableName
  char '.'
  funcName <- letter `manyTill` (char '(')
  funcArgs <- anyChar `manyTill` (char ')')
  return $ FunctionCall (Just obj) (SimpleVar funcName) (split "," funcArgs)

functionCallWithoutArgs = debug "functionCallWithArgs" >> do
  obj <- variableName
  char '.'
  funcName <- letter `manyTill` (lookAhead . try $ (oneOf " .),\n;"))
  return $ FunctionCall (Just obj) (SimpleVar funcName) []

functionCallWithoutObject = debug "functionCallWithoutObject" >> do
  funcName <- letter `manyTill` (char '(')
  funcArgs <- anyChar `manyTill` (char ')')
  return $ FunctionCall Nothing (SimpleVar funcName) (split "," funcArgs)
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
