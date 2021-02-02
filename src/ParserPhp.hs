module ParserPhp where

import Types
import Utils
import Formatting
import Text.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.Combinator
import Debug.Trace (trace)
import PhpToPhp

data PhpState = PhpState {
                      lastPhp :: Salty,
                      stateScopes :: [Scope]
                  }

type PhpParser = Parsec String PhpState Salty

debug :: String -> PhpParser
debug str = return (SaltyString str)
-- debug str = parserTrace str >> return (SaltyString str)

phpToPhp :: Int -> String -> String
phpToPhp indentAmt str = case (build str) of
                   Left err -> show err
                   Right xs -> phpToPhp_ indentAmt xs
                   -- Right xs -> checkForErrors str (saltyToPhp_ indentAmt xs)

numLines str = length . filter (/="") . lines $ str

checkForErrors inputStr outputStr = if (numLines outputStr) < (numLines inputStr)
                                  then outputStr ++ "\n// failed, possibly on:\n" ++ (findErrorLine inputStr)
                                  else outputStr

findErrorLine :: String -> String
findErrorLine str = findErrorLine_ ((map (removeSemicolons . strip)) . lines $ str)

findErrorLine_ :: [String] -> String
findErrorLine_ [] = "// no errors"
findErrorLine_ lines = case (build (head lines)) of
                         Left err -> "error from adit" -- this never gets hit, not sure why.
                         Right [] -> head lines -- this means the parse failed, so this is the issue line.
                         Right xs -> findErrorLine_ (tail lines) -- this means the parse succeeded, so try the next line.

phpToDebugTree :: String -> String
phpToDebugTree str = case (build str) of
                   Left err -> show err
                   Right xs -> formatDebug xs

startingState = PhpState EmptyLine []

build :: String -> Either ParseError [Salty]
build str_ = runParser phpParser startingState "phpParser" str
  where str = unlines . (map (removeSemicolons . strip)) . lines $ str_

phpParser :: Parsec String PhpState [Salty]
phpParser = debug "start" >> (many phpParserSingle)

phpParserSingle :: PhpParser
phpParserSingle = debug "phpParserSingle" >> do
  salty <- phpParserSingle_
  debug $ "checking for newline: " ++ (show salty)
  result <- optionMaybe $ char '\n'
  case result of
       Nothing -> return salty
       (Just s) -> do
          debug $ "found a newline!" ++ [s]
          return (WithNewLine salty)

phpParserSingle_ :: PhpParser
phpParserSingle_ = do
  debug "phpParserSingle_"
  salty <- phpParserSingleWithoutNewline
  if worthSaving salty
     then modifyState (saveIt salty)
     else return ()
  return salty

worthSaving EmptyLine = False
worthSaving _ = True

saveIt :: Salty -> PhpState -> PhpState
saveIt salty (PhpState _ scopes) = PhpState salty scopes

phpParserSingleWithoutNewline :: PhpParser
phpParserSingleWithoutNewline = do
  operation
  <||> partialOperation
  <||> phpString
  <||> phpNumber
  <||> variable
  <||> purePhp
  <||> emptyLine

safeHead [] = Nothing
safeHead (x:xs) = Just x

operator = debug "operator" >> do
       (string "!=" >> return NotEquals)
  <||> (string "+=" >> return PlusEquals)
  <||> (string "-=" >> return MinusEquals)
  <||> (string "/=" >> return DivideEquals)
  <||> (string "*=" >> return MultiplyEquals)
  <||> (string "[]=" >> return ArrayPush)
  <||> (string "||=" >> return OrEquals)
  <||> (string "||" >> return OrOr)
  <||> (string "&&" >> return AndAnd)
  <||> (string "??" >> return NullCoalesce)
  <||> (string "++" >> return PlusPlus)
  <||> (string "<>" >> return ArrayMerge)
  <||> (string "<->" >> return ArrayDiff)
  <||> (string "instanceof" >> return InstanceOf)
  <||> (string "isa" >> return InstanceOf)
  <||> (string "in" >> return In)
  <||> (string "keyin" >> return KeyIn)
  <||> (string "==" >> return EqualsEquals)
  <||> (string "<=>" >> return Spaceship)
  <||> (string "<=" >> return LessThanOrEqualTo)
  <||> (string ">=" >> return GreaterThanOrEqualTo)
  <||> (string "<" >> return LessThan)
  <||> (string ">" >> return GreaterThan)
  <||> (string "+" >> return Add)
  <||> (string "-" >> return Subtract)
  <||> (string "/" >> return Divide)
  <||> (string "*" >> return Multiply)
  <||> (string "=" >> return Equals)
  <||> (string "%" >> return Modulo)

operation = debug "operation" >> do
  left <- variable
  space
  op <- operator
  space
  right <- phpParserSingle
  return $ Operation left op right

partialOperation = debug "partialOperation" >> do
  leftHandSide <- lastPhp <$> getState
  space
  op <- operator
  space
  right <- phpParserSingle
  return $ BackTrack (Operation leftHandSide op right)

phpString = debug "phpString" >> do
  oneOf "'\""
  str <- many $ noneOf "\"'"
  oneOf "'\""
  return $ SaltyString str

phpNumber = debug "phpNumber" >> do
       decimal
  <||> integer

integer = debug "integer" >> do
  head <- oneOf "1234567890-"
  number <- many (oneOf "1234567890")
  return $ SaltyNumber (head:number)

decimal = debug "decimal" >> do
  head <- oneOf "1234567890-"
  number <- many (oneOf "1234567890")
  char '.'
  decimal <- many1 (oneOf "1234567890")
  return $ SaltyNumber ((head:number) ++ "." ++ decimal)

constant = debug "constant" >> do
  string "const"
  space
  varName <- many1 upper
  return $ Constant (Variable (SimpleVar varName) GlobalScope)

variable = debug "variable" >> do
  constant

emptyLine = debug "emptyLine" >> do
  string "\n"
  return EmptyLine

purePhp = do
  line <- many1 $ noneOf "\n"
  return $ PurePhp line
