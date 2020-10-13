{-# LANGUAGE FlexibleContexts #-}

module Lib where

import Types
import Text.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.Combinator
--import Text.ParserCombinators.Parsec.Error(messageString)
import Data.List (intercalate)
import qualified Data.Functor.Identity
import Utils
import System.IO.Unsafe
import System.Environment
import Debug.Trace (trace)

debug :: String -> ParsecT String u Data.Functor.Identity.Identity Salty
debug str = trace str $ return (SaltyString str)
-- debug str = return $ unsafePerformIO $ do
--   debugFlag <- lookupEnv "DEBUG"
--   case debugFlag of
--     Nothing -> return (SaltyString $ "debug flag off for debug: " ++ str)
--     Just _ -> putStrLn str >> return (SaltyString $ "debug: " ++ str)

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

-- getRight (Right x) = x
-- getRight (Left x) = SaltyString $ "error adit: " ++ (show x)

-- parse_ :: String -> Either ParseError Salty
parse_ :: String -> String -> ParsecT String u Data.Functor.Identity.Identity Salty
parse_ "" caller = return EmptyLine
parse_ str caller = case parsed of
              (Left err) -> parserTraced (show err) (unexpected (show err))
              (Right s) -> return s
  where newStr = if (last str == '\n') then str else (str ++ "\n")
        parsed = parse saltyParserSingle ("parse_ from " ++ caller) newStr

saltyParser :: ParsecT String u Data.Functor.Identity.Identity [Salty]
saltyParser = many saltyParserSingle

saltyParserSingle :: ParsecT String u Data.Functor.Identity.Identity Salty
saltyParserSingle = debug "saltyParserSingle" >> do
  parens
  <||> function
  <||> operation
  <||> saltyString
  <||> saltyNumber
  <||> returnStatement
  <||> higherOrderFunctionCall
  <||> functionCall
  <||> negateSalty
  <||> emptyLine

variableName = debug "variableName" >> do
        classVar
  <||>  instanceVar
  <||>  simpleVar

-- functionBody = debug "functionBody" >> do
--         lambda
--   <||>  ampersand
--   <||>  oneLine

parens = debug "parens" >> do
  char '('
  body <- saltyParserSingle
  char ')'
  return $ Parens body

parensWith :: ParsecT String u Data.Functor.Identity.Identity Salty -> ParsecT String u Data.Functor.Identity.Identity Salty
parensWith parser = debug "parensWith" >> do
  char '('
  body <- parser
  char ')'
  return $ Parens body

function = debug "function" >> do
  parserTrace "1"
  name <- variableName
  parserTrace "2"
  args <- anyToken `manyTill` (string ":=")
  parserTrace "3"
  space
  parserTrace "4"
  body_ <- anyChar `manyTill` char ';'
  parserTrace $ "5" ++ body_
  body <- parse_ body_ "function"
  return $ Function name (map argWithDefaults (words args)) body

operator = debug "operator" >> do
       (string "+" >> return Add)
  <||> (string "+" >> return Subtract)
  <||> (string "/" >> return Divide)
  <||> (string "*" >> return Multiply)
  <||> (string "=" >> return Equals)
  <||> (string "!=" >> return NotEquals)
  <||> (string "+=" >> return PlusEquals)
  <||> (string "-=" >> return MinusEquals)
  <||> (string "/=" >> return DivideEquals)
  <||> (string "*=" >> return MultiplyEquals)
  <||> (string "||=" >> return OrEquals)
  <||> (string "||" >> return OrOr)
  <||> (string "&&" >> return AndAnd)

atom = debug "atom" >> do
       (Left <$> variableName)
  <||> (Right <$> saltyString)
  <||> (Right <$> saltyNumber)

operation = debug "operation" >> do
  parserTrace "a"
  left <- atom
  space
  parserTrace "b"
  op <- operator
  space
  parserTrace "c"
  right <- atom
  parserTrace "d"
  return $ Operation left op right

negateSalty = debug "negateSalty" >> do
  char '!'
  s <- saltyParserSingle
  return $ Negate s

emptyLine = debug "emptyLine" >> do
  string "\n\n"
  return EmptyLine

betweenQuotes = between (oneOf "\"'") (oneOf "\"'")

saltyString = debug "saltyString" >> do
  str <- betweenQuotes (many anyToken)
  return $ SaltyString str

saltyNumber = debug "saltyNumber" >> do
  number <- many1 (oneOf "1234567890.")
  return $ SaltyNumber number

-- @foo
instanceVar = debug "instanceVar" >> do
  char '@'
  variable <- letter `manyTill` (lookAhead . try $ (oneOf " .),\n;"))
  return $ InstanceVar variable

-- @@foo
classVar = debug "classVar" >> do
  string "@@"
  variable <- letter `manyTill` (lookAhead . try $ (oneOf " .),\n;"))
  return $ ClassVar variable

-- foo
simpleVar = debug "simpleVar" >> do
  variable <- letter `manyTill` (lookAhead . try $ (oneOf " .),\n;"))
  return $ SimpleVar variable

-- block = do
--   string "do"
--   newline
--   blockLines <- anyToken `manyTill` (string "end")
--   let l = lines blockLines
--   return $ Block (map parse_ l)

lambda = debug "lambda" >> do
  string "\\"
  args <- anyToken `manyTill` (string "->")
  spaces
  body_ <- anyToken `manyTill` (char '#')
  body <- parse_ body_ "lambda"
  return $ LambdaFunction (words args) body

-- ampersand = debug "ampersand" >> do
--   char '&'
--   var <- variableName
--   return $ AmpersandFunction var

returnStatement = debug "returnStatement" >> do
  string "return "
  line <- anyToken `manyTill` (try newline)
  salty <- parse_ line "returnStatement"
  return $ ReturnStatement salty

hashLookup = debug "hashLookup" >> do
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

higherOrderFunction = debug "higherOrderFunction" >> do
       eachFunc
  <||> mapFunc
  <||> selectFunc
  <||> anyFunc
  <||> allFunc

lastMatch ch = do
  list <- many1 (char ch)
  return $ (init list)

manyTillChar ch = do
  body <- anyToken `manyTill` (lookAhead . try $ char ch)
  end <- lastMatch ')'
  return (body ++ end)

higherOrderFunctionCall = debug "higherOrderFunctionCall" >> do
  obj <- variableName
  hof <- higherOrderFunction
  (Parens func) <- parensWith lambda
  return $ HigherOrderFunctionCall obj hof func

-- phpLine = do
--   line <- anyChar `manyTill` (string "\n")
--   return $ PhpLine line

functionCall = debug "functionCall" >> do
       functionCallOnObject
  <||> functionCallWithoutObject

functionCallOnObject = debug "functionCallOnObject" >> do
  obj <- variableName
  char '.'
  funcName <- letter `manyTill` (char '(')
  funcArgs <- anyChar `manyTill` (char ')')
  return $ FunctionCall (Just obj) (SimpleVar funcName) (split "," funcArgs)

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
