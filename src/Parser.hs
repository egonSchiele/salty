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

varNameChars = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"
functionArgsChars = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_"
hashKeyChars = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_'\""
constChars = oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ_"
typeChars = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_?"
flagNameChars = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_.1234567890"

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

worthSaving EmptyLine = False
worthSaving _ = True

saltyParserSingle_ :: SaltyParser
saltyParserSingle_ = do
  debug "saltyParserSingle_"
  salty <- saltyParserSingle__
  if worthSaving salty
     then putState salty
     else return ()
  return salty

saltyParserSingle__ :: SaltyParser
saltyParserSingle__ = do
  parens
  <||> hashTable
  <||> array
  <||> braces
  <||> function
  <||> functionTypeSignature
  <||> constant
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
  <||> flagName
  <||> emptyLine
  <||> ifStatement
  <||> whileStatement
  <||> classDefinition
  <||> objectCreation
  <||> saltyBool
  <||> saltyNull
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

validHashValue = debug "validHashValue" >> do
       saltyString
  <||> saltyNumber
  <||> hashLookup
  <||> hashTable
  <||> array
  <||> flagName
  <||> saltyBool
  <||> saltyNull
  <||> variable

keyValuePair = debug "keyValuePair" >> do
  key <- many1 hashKeyChars
  char ':'
  space
  value <- validHashValue
  char ','
  optional (oneOf " \n")
  return (key, value)

hashTable = debug "hashTable" >> do
  char '{'
  optional (oneOf " \n")
  kvPairs <- many1 keyValuePair
  optional (oneOf " \n")
  char '}'
  return $ HashTable kvPairs

arrayValue = debug "arrayValue" >> do
  value <- validHashValue
  char ','
  optional space
  return value

array = debug "array" >> do
  char '['
  salties <- many arrayValue
  char ']'
  return $ Array salties

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

functionArgs = debug "functionArgs" >> many (do
  arg <- many1 functionArgsChars
  space
  return arg)

onelineFunction = debug "onelineFunction" >> do
  prevSalty <- getState
  name_ <- variableName
  let (name, visibility) = getVisibility name_
  space
  args <- functionArgs
  string ":="
  space
  body_ <- many1 (noneOf "\n")
  optional $ char '\n'
  case (build body_) of
       Left err -> return $ SaltyString (show err)
       Right body -> do
          case prevSalty of
               FunctionTypeSignature _ types ->
                  return $ Function name (argWithTypes args types) body visibility
               _ ->
                  return $ Function name (map argWithDefaults args) body visibility

multilineFunction = debug "multilineFunction" >> do
  prevSalty <- getState
  name_ <- variableName
  let (name, visibility) = getVisibility name_
  space
  args <- functionArgs
  string ":="
  space
  body <- braces
  case prevSalty of
     FunctionTypeSignature _ types -> return $ Function name (argWithTypes args types) [body] visibility
     _ -> return $ Function name (map argWithDefaults args) [body] visibility

argWithDefaults name = Argument Nothing name Nothing
argWithTypes [] _ = []
argWithTypes (a:args) [] = (Argument Nothing a Nothing):(argWithTypes args [])
argWithTypes (a:args) (t:types) = (Argument (Just t) a Nothing):(argWithTypes args types)

getVisibility :: VariableName  -> (VariableName, Visibility)
getVisibility (InstanceVar name) = ((InstanceVar newName), visibility)
  where (visibility, newName) = _getVisibility name
getVisibility (StaticVar name) = ((StaticVar newName), visibility)
  where (visibility, newName) = _getVisibility name
getVisibility (ClassVar name) = ((ClassVar newName), visibility)
  where (visibility, newName) = _getVisibility name
getVisibility (SimpleVar name) = ((SimpleVar newName), visibility)
  where (visibility, newName) = _getVisibility name

_getVisibility :: String -> (Visibility, String)
_getVisibility "__construct" = (Public, "__construct")
_getVisibility ('_':xs) = (Private, xs)
_getVisibility str = (Public, str)

getVarName :: VariableName -> String
getVarName (InstanceVar str) = str
getVarName (StaticVar str) = str
getVarName (ClassVar str) = str
getVarName (SimpleVar str) = str

findArgTypes = debug "findArgTypes" >> do
  args <- many1 $ do
            typ <- many1 typeChars
            optional $ string " -> "
            if (head typ == '?')
               then return (ArgumentType True (tail typ))
               else return (ArgumentType False typ)
  return args

functionTypeSignature = debug "functionTypeSignature" >> do
  name <- variableName
  space
  string "::"
  space
  argTypes <- findArgTypes
  return $ FunctionTypeSignature name argTypes

operator = debug "operator" >> do
       (string "!=" >> return NotEquals)
  <||> (string "+=" >> return PlusEquals)
  <||> (string "-=" >> return MinusEquals)
  <||> (string "/=" >> return DivideEquals)
  <||> (string "*=" >> return MultiplyEquals)
  <||> (string "||=" >> return OrEquals)
  <||> (string "||" >> return OrOr)
  <||> (string "&&" >> return AndAnd)
  <||> (string "??" >> return NullCoalesce)
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

constant = debug "constant" >> do
  name_ <- many1 constChars
  let (visibility, name) = _getVisibility name_
  space
  char '='
  space
  value <- (saltyString <||> saltyNumber <||> saltyBool <||> saltyNull)
  return $ Constant visibility name value

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

flagName = do
  char '~'
  name <- many1 flagNameChars
  return $ FlagName name

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

saltyBool = debug "saltyBool" >> (saltyTrue <||> saltyFalse)

saltyTrue = debug "saltyTrue" >> do
  s <- string "true"
  return $ SaltyBool TRUE

saltyFalse = debug "saltyFalse" >> do
  s <- string "false"
  return $ SaltyBool FALSE

saltyNull = debug "saltyNull" >> do
  s <- string "null"
  return SaltyNull
