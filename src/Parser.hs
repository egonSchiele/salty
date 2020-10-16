module Parser where

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
import ToPhp

type SaltyState = Salty
type SaltyParser = Parsec String SaltyState Salty

debug :: String -> SaltyParser
debug str = return (SaltyString str)
-- debug str = parserTrace str >> return (SaltyString str)

saltyToPhp :: String -> String
saltyToPhp str = case (build str) of
                   Left err -> show err
                   Right xs -> saltyToPhp_ xs

saltyToDebugTree :: String -> String
saltyToDebugTree str = case (build str) of
                   Left err -> show err
                   Right xs -> formatDebug xs

build :: String -> Either ParseError [Salty]
build str_ = runParser saltyParser EmptyLine "saltyParser" str
  where str = unlines . (map strip) . lines $ str_

saltyParser :: Parsec String SaltyState [Salty]
saltyParser = debug "start" >> (many saltyParserSingle)

saltyParserSingle :: SaltyParser
saltyParserSingle = debug "saltyParserSingle" >> do
  salty <- saltyParserSingle_
  debug $ "checking for newline: " ++ (show salty)
  result <- optionMaybe $ char '\n'
  case result of
       Nothing -> return salty
       (Just s) -> do
         debug $ "found a newline!" ++ [s]
         return (WithNewLine salty)

saltyParserSingle_ :: SaltyParser
saltyParserSingle_ = do
  debug "saltyParserSingle_"
  salty <- saltyParserSingle__
  putState salty
  return salty

saltyParserSingle__ :: SaltyParser
saltyParserSingle__ = do
  parens
  <||> braces
  <||> function
  <||> operation
  <||> partialOperation
  <||> saltyString
  <||> saltyNumber
  <||> returnStatement
  -- <||> higherOrderFunctionCall
  <||> functionCall
  <||> hashLookup
  <||> partialHashLookup
  <||> negateSalty
  <||> saltyComment
  <||> phpComment
  <||> phpLine
  <||> emptyLine
  <||> ifStatement
  <||> whileStatement
  <||> classDefinition
  <||> objectCreation
  <||> variable

variable = debug "variable" >> do
  name <- variableName
  return $ Variable name

variableName = debug "variableName" >> do
        staticVar
  <||>  instanceVar
  <||>  classVar
  <||>  simpleVar

parens = debug "parens" >> do
  char '('
  body <- saltyParser
  char ')'
  debug $ "parens done with: " ++ (show body)
  return $ Parens body

braces = debug "braces" >> do
  char '{'
  optional space
  body <- saltyParser
  optional space
  char '}'
  debug $ "braces done with: " ++ (show body)
  return $ Braces body

parensWith :: SaltyParser -> SaltyParser
parensWith parser = debug "parensWith" >> do
  char '('
  body <- parser
  char ')'
  return $ Parens [body]

function = debug "function" >> do
  multilineFunction <||> onelineFunction

onelineFunction = debug "onelineFunction" >> do
  name <- variableName
  space
  args <- many (letter <|> digit <|> space <|> (char '_'))
  string ":="
  space
  body <- many1 saltyParserSingle_
  optional $ char '\n'
  return $ Function name (map argWithDefaults (words args)) body

multilineFunction = debug "multilineFunction" >> do
  name <- variableName
  space
  args <- many (letter <|> digit <|> space <|> (char '_'))
  string ":="
  space
  body <- braces
  return $ Function name (map argWithDefaults (words args)) [body]

operator = debug "operator" >> do
       (string "!=" >> return NotEquals)
  <||> (string "+=" >> return PlusEquals)
  <||> (string "-=" >> return MinusEquals)
  <||> (string "/=" >> return DivideEquals)
  <||> (string "*=" >> return MultiplyEquals)
  <||> (string "||=" >> return OrEquals)
  <||> (string "||" >> return OrOr)
  <||> (string "&&" >> return AndAnd)
  <||> (string "++" >> return PlusPlus)
  <||> (string "==" >> return EqualsEquals)
  <||> (string "<=" >> return LessThanOrEqualTo)
  <||> (string ">=" >> return GreaterThanOrEqualTo)
  <||> (string "<" >> return LessThan)
  <||> (string ">" >> return GreaterThan)
  <||> (string "+" >> return Add)
  <||> (string "-" >> return Subtract)
  <||> (string "/" >> return Divide)
  <||> (string "*" >> return Multiply)
  <||> (string "=" >> return Equals)

atom = debug "atom" >> do
       variable
  <||> saltyString
  <||> saltyNumber

operation = debug "operation" >> do
  left <- atom
  debug $ "op left: " ++ (show left)
  space
  op <- operator
  space
  right <- (saltyParserSingle <||> atom)
  debug $ "op right: " ++ (show right)
  return $ Operation left op right

partialOperation = debug "partialOperation" >> do
  leftHandSide <- getState
  space
  op <- operator
  space
  right <- (saltyParserSingle <||> atom)
  return $ BackTrack (Operation leftHandSide op right)

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
  number <- many1 (oneOf "1234567890.-")
  return $ SaltyNumber number

varNameChars = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"

-- @foo
instanceVar = debug "instanceVar" >> do
  char '@'
  variable <- many1 varNameChars
  lookAhead $ oneOf endDelim
  return $ InstanceVar variable

-- @@foo
staticVar = debug "staticVar" >> do
  string "@@"
  variable <- many1 varNameChars
  lookAhead $ oneOf endDelim
  return $ StaticVar variable

-- foo
simpleVar = debug "simpleVar" >> do
  variable <- many1 varNameChars
  lookAhead $ oneOf endDelim
  return $ SimpleVar variable

-- @@foo
classVar = debug "classVar" >> do
  start <- upper
  variable <- many1 varNameChars
  lookAhead $ oneOf endDelim
  return $ ClassVar (start:variable)

lambda = debug "lambda" >> do
  string "\\"
  args <- anyToken `manyTill` (string " -> ")
  body <- saltyParserSingle
  return $ LambdaFunction (words args) body

returnStatement = debug "returnStatement" >> do
  string "return "
  salty <- many1 saltyParserSingle_
  optional $ char '\n'
  return $ ReturnStatement (Braces salty)

-- eachFunc = string ".each" >> return Each
-- mapFunc = string ".map" >> return Map
-- selectFunc = string ".select" >> return Select
-- anyFunc = string ".any" >> return Any
-- allFunc = string ".all" >> return All

-- higherOrderFunction = debug "higherOrderFunction" >> do
--        eachFunc
--   <||> mapFunc
--   <||> selectFunc
--   <||> anyFunc
--   <||> allFunc

-- higherOrderFunctionCall = debug "higherOrderFunctionCall" >> do
--   obj <- variable
--   hof <- higherOrderFunction
--   (Parens func) <- parensWith lambda
--   return $ HigherOrderFunctionCall obj hof func

saltyComment = do
  char '#'
  line <- anyChar `manyTill` (string "\n")
  return $ SaltyComment line

phpComment = do
  string "// "
  line <- anyChar `manyTill` (string "\n")
  return $ PhpComment line

phpLine = do
  string "```"
  line <- many1 anyChar
  string "```"
  return $ PhpLine line

functionCall = debug "functionCall" >> do
       functionCallOnObject
  <||> functionCallWithoutObject

endDelim = " .(),\n;[]"

findArgs = debug "findArgs" >> do
  args <- many $ do
            s <- saltyParserSingle_
            optional $ char ','
            many space
            return s
  return args

functionCallOnObject = debug "functionCallOnObject" >> do
  obj <- variable
  char '.'
  funcName <- varNameChars `manyTill` (char '(')
  funcArgs <- findArgs
  char ')'
  return $ FunctionCall (Just obj) (Right (SimpleVar funcName)) funcArgs

parseBuiltInFuncName :: VariableName-> Either BuiltInFunction VariableName
parseBuiltInFuncName (SimpleVar "p") = Left VarDumpShort
parseBuiltInFuncName s = Right s

functionCallWithoutObject = debug "functionCallWithoutObject" >> do
  funcName <- variableName
  char '('
  funcArgs <- findArgs
  char ')'
  return $ FunctionCall Nothing (parseBuiltInFuncName funcName) funcArgs

hashLookup = debug "hashLookup" >> do
  hash <- variable
  char '['
  key <- saltyParserSingle
  char ']'
  return $ HashLookup hash key

partialHashLookup = debug "partialHashLookup" >> do
  hash <- getState
  char '['
  key <- saltyParserSingle
  char ']'
  return $ BackTrack (HashLookup hash key)

ifStatement = debug "ifStatement" >> do
  ifWithElse <||> ifWithoutElse

ifWithElse = debug "ifWithElse" >> do
  string "if"
  space
  condition <- saltyParserSingle
  space
  string "then"
  space
  thenFork <- saltyParserSingle
  space
  string "else"
  space
  elseFork <- saltyParserSingle
  return $ If condition thenFork (Just elseFork)

ifWithoutElse = debug "ifWithoutElse" >> do
  string "if"
  space
  condition <- saltyParserSingle
  space
  string "then"
  space
  thenFork <- saltyParserSingle
  return $ If condition thenFork Nothing

whileStatement = debug "whileStatement" >> do
  string "while"
  space
  condition <- saltyParserSingle
  space
  body <- saltyParserSingle
  return $ While condition body

classDefinition = debug "classDefinition" >> do
  string "class"
  space
  name <- classVar
  space
  body <- braces
  return $ Class name body

objectCreation = debug "objectCreation" >> do
  string "new"
  space
  className <- classVar
  char '('
  constructorArgs <- findArgs
  char ')'
  return $ New className constructorArgs
