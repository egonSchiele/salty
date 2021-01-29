module Parser where

import Types
import Utils
import Formatting
import Text.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.Combinator
import Debug.Trace (trace)
import ToPhp

varNameChars = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_"
classNameChars = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_\\"
functionArgsChars = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_&"
hashKeyChars = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_'\""
constChars = oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ_"
typeChars = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_?[]"
flagNameChars = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_.1234567890"

type SaltyState = Salty
type SaltyParser = Parsec String SaltyState Salty

debug :: String -> SaltyParser
debug str = return (SaltyString str)
-- debug str = parserTrace str >> return (SaltyString str)

saltyToPhp :: Int -> String -> String
saltyToPhp indentAmt str = case (build str) of
                   Left err -> show err
                   Right xs -> saltyToPhp_ indentAmt xs

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
  salty <- saltyParserSingleWithoutNewline
  if worthSaving salty
     then putState salty
     else return ()
  return salty

worthSaving EmptyLine = False
worthSaving _ = True

saltyParserSingleWithoutNewline :: SaltyParser
saltyParserSingleWithoutNewline = do
  parens
  <||> hashTable
  <||> array
  <||> braces
  <||> function
  <||> functionTypeSignature
  <||> higherOrderFunctionCall
  <||> lambda
  <||> constant
  <||> multiAssign
  <||> plusplusAll
  <||> operation
  <||> partialOperation
  <||> saltyString
  <||> range
  <||> saltyNumber
  <||> returnStatement
  <||> functionCall
  <||> attrAccess
  <||> arraySlice
  <||> hashLookup
  <||> partialHashLookup
  <||> partialFunctionCall
  <||> partialAttrAccess
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
  <||> saltyKeyword
  <||> saltyMagicConstant
  <||> variable
  -- <||> parseError

validFuncArgTypes :: SaltyParser
validFuncArgTypes = debug "validFuncArgTypes" >> do
       hashTable
  <||> constant
  <||> array
  <||> operation
  <||> partialOperation
  <||> saltyString
  <||> saltyNumber
  <||> functionCall
  <||> attrAccess
  <||> arraySlice
  <||> hashLookup
  <||> partialHashLookup
  <||> partialFunctionCall
  <||> partialAttrAccess
  <||> negateSalty
  <||> flagName
  <||> objectCreation
  <||> saltyBool
  <||> saltyNull
  <||> saltyMagicConstant
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
  <||> saltyMagicConstant
  <||> variable

keyValuePair = debug "keyValuePair" >> do
  key <- many1 hashKeyChars
  char ':'
  space
  value <- validHashValue
  char ',' <||> char '}' <||> char '\n'
  optional (oneOf " \n")
  return (key, value)

hashTable = debug "hashTable" >> do
  char '{'
  optional (oneOf " \n")
  kvPairs <- many1 keyValuePair
  optional $ char '}'
  return $ HashTable kvPairs

arrayValue = debug "arrayValue" >> do
  value <- validHashValue
  char ',' <||> char ']'
  optional space
  return value

array = debug "array" >> do
  char '['
  salties <- many arrayValue
  optional $ char ']'
  return $ Array salties

braces = debug "braces" >> do
  char '{'
  optional space
  body <- saltyParser
  optional space
  char '}'
  debug $ "braces done with: " ++ (show body)
  return $ Braces body

function = debug "function" >> do
  multilineFunction <||> onelineFunction

functionArgs = debug "functionArgs" >> many (do
  arg <- many1 functionArgsChars
  space
  return arg)

makeArgNames :: [String] -> [ArgumentName]
makeArgNames [] = []
makeArgNames (arg:rest)
  | head arg == '&' = (ArgumentName (tail arg) True):(makeArgNames rest)
  | otherwise       = (ArgumentName arg False):(makeArgNames rest)

onelineFunction = debug "onelineFunction" >> do
  prevSalty <- getState
  (name, visibility) <- getVisibility <$> variableName
  space
  _args <- functionArgs
  let args = makeArgNames _args
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
  (name, visibility) <- getVisibility <$> variableName
  space
  _args <- functionArgs
  let args = makeArgNames _args
  string ":="
  space
  body <- braces
  case prevSalty of
     FunctionTypeSignature _ types -> return $ Function name (argWithTypes args types) [body] visibility
     _ -> return $ Function name (map argWithDefaults args) [body] visibility

argWithDefaults :: ArgumentName -> Argument
argWithDefaults name = Argument Nothing name Nothing

argWithTypes :: [ArgumentName] -> [ArgumentType] -> [Argument]
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
            if (last typ == '?')
               then return (ArgumentType True (init typ) False)
               else return (ArgumentType False typ False)
  return $ (init args) ++ [setToReturnArgType (last args)]

setToReturnArgType (ArgumentType o a _) = ArgumentType o a True

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

atom = debug "atom" >> do
       functionCall
  <||> constant
  <||> attrAccess
  <||> arraySlice
  <||> hashLookup
  <||> saltyBool
  <||> saltyNull
  <||> saltyMagicConstant
  <||> variable
  <||> saltyString
  <||> saltyNumber

constant = debug "constant" >> do
  (visibility, name) <- _getVisibility <$> (many1 constChars)
  space
  char '='
  space
  value <- (saltyString <||> saltyNumber <||> saltyBool <||> saltyNull)
  return $ Constant visibility name value

operation = debug "operation" >> do
  left <- atom
  space
  op <- operator
  space
  right <- saltyParserSingle
  return $ Operation left op right

partialOperation = debug "partialOperation" >> do
  leftHandSide <- getState
  space
  op <- operator
  space
  right <- saltyParserSingle
  return $ BackTrack (Operation leftHandSide op right)

partialAttrAccess = debug "partialAttrAccess" >> do
  leftHandSide <- getState
  char '.'
  attrName <- many1 varNameChars
  return $ BackTrack (AttrAccess leftHandSide attrName)

partialFunctionCall = debug "partialFunctionCall" >> do
  leftHandSide <- getState

  char '.'
  funcName <- many1 varNameChars
  char '('
  funcArgs <- findArgs
  char ')'

  return $ case leftHandSide of
             Operation l op r -> BackTrack $ Operation l op (FunctionCall (Just r) (Right (SimpleVar funcName)) funcArgs)
             _ -> BackTrack $ FunctionCall (Just leftHandSide) (Right (SimpleVar funcName)) funcArgs

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

-- @foo
instanceVar = debug "instanceVar" >> do
  char '@'
  variable <- many1 varNameChars
  return $ InstanceVar variable

-- @@foo
staticVar = debug "staticVar" >> do
  string "@@"
  variable <- many1 varNameChars
  return $ StaticVar variable

-- foo
simpleVar = debug "simpleVar" >> do
  first <- (letter <||> char '_')
  rest <- many varNameChars
  return $ SimpleVar (first:rest)

-- Foo
classVar = debug "classVar" >> do
  start <- upper
  variable <- many1 classNameChars
  return $ ClassVar (start:variable)

higherOrderFunctionCall = debug "higherOrderFunctionCall" >> do
  optional $ char '('
  obj <- range <||> variable
  optional $ char ')'
  char '.'
  funcName <-      (string "map" >> return Map)
              <||> (string "each" >> return Each)
              <||> (string "select" >> return Select)
              <||> (string "any" >> return Any)
              <||> (string "all" >> return All)
  char '('
  func <- lambda
  char ')'
  return $ HigherOrderFunctionCall obj funcName func "$result"

lambda = debug "lambda" >> do
  char '\\'
  args <- anyToken `manyTill` (string " -> ")
  body <- saltyParserSingle
  return $ LambdaFunction (words args) body

returnStatement = debug "returnStatement" >> do
  string "return "
  salty <- many1 saltyParserSingle_
  optional $ char '\n'
  return $ ReturnStatement (Braces salty)

saltyComment = do
  char '#'
  line <- many1 $ noneOf "\n"
  string "\n"
  return $ SaltyComment line

phpComment = do
  string "// "
  line <- many1 $ noneOf "\n"
  string "\n"
  return $ PhpComment line

phpLine = do
  string "```"
  line <- many1 $ noneOf "`"
  string "```"
  return $ PhpLine line

flagName = do
  char '~'
  name <- many1 flagNameChars
  return $ FlagName name

functionCall = debug "functionCall" >> do
       functionCallOnObject
  <||> functionCallWithoutObject

findArgs = debug "findArgs" >> do
  args <- many $ do
            s <- validFuncArgTypes
            optional $ char ','
            many space
            return s
  return args

attrAccess = debug "attrAccess" >> do
  obj <- variable
  char '.'
  attrName <- many1 varNameChars
  return $ AttrAccess obj attrName

functionCallOnObject = debug "functionCallOnObject" >> do
  obj <- variable
  char '.'
  funcName <- many1 varNameChars
  char '('
  funcArgs <- findArgs
  char ')'
  return $ FunctionCall (Just obj) (Right (SimpleVar funcName)) funcArgs

parseBuiltInFuncName :: VariableName -> Either BuiltInFunction VariableName
parseBuiltInFuncName (SimpleVar "p") = Left VarDumpShort
parseBuiltInFuncName s = Right s

functionCallWithoutObject = debug "functionCallWithoutObject" >> do
  funcName <- variableName
  char '('
  funcArgs <- findArgs
  char ')'
  return $ FunctionCall Nothing (parseBuiltInFuncName funcName) funcArgs

arraySlice = debug "arraySlice" >> do
  array <- variable
  char '['
  start_ <- (Just <$> saltyParserSingle) <||> nothing
  char ':'
  end <- (Just <$> saltyParserSingle) <||> nothing
  char ']'
  let start = case start_ of
                Just salty -> salty
                Nothing -> SaltyNumber "0"
  return $ ArraySlice array start end

hashLookup = debug "hashLookup" >> do
       shortHashLookup
  <||> standardHashLookup

shortHashLookup = debug "shortHashLookup" >> do
  char ':'
  hash <- variable
  keys <- many1 $ hashKeyNumber <||> hashKeyString
  return $ foldl (\acc key -> HashLookup acc key) (HashLookup hash (head keys)) (tail keys)

hashKeyNumber = debug "hashKeyString" >> do
  char '.'
  key <- many1 digit
  return $ SaltyNumber key

hashKeyString = debug "hashKeyString" >> do
  char '.'
  key <- many1 varNameChars
  return $ SaltyString key

standardHashLookup = debug "standardHashLookup" >> do
  hash <- variable
  char '['
  key <- validFuncArgTypes
  char ']'
  return $ HashLookup hash key

partialHashLookup = debug "partialHashLookup" >> do
  hash <- getState
  char '['
  key <- validFuncArgTypes
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
  body <- braces
  return $ While condition body


classDefinition = debug "classDefinition" >> do
  string "class"
  space
  name <- classVar
  space
  extendsName <- classDefExtends <||> nothing
  implementsName <- classDefImplements <||> nothing
  optional space
  body <- braces
  return $ Class name extendsName implementsName body

classDefExtends = debug "classDefExtends" >> do
  string "extends"
  space
  extendsName <- classVar
  space
  return $ Just extendsName

classDefImplements = debug "classDefImplements" >> do
  string "implements"
  space
  implementsName <- classVar
  space
  return $ Just implementsName

nothing = return Nothing

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

saltyKeyword = debug "saltyKeyword" >> do
  phpKeyword <-      saltyKeywordUse
                <||> saltyKeywordThrow
                <||> saltyKeywordRequire
                <||> saltyKeywordRequireOnce
                <||> saltyKeywordNamespace
  return $ Keyword phpKeyword

saltyKeywordUse = debug "saltyKeywordUse" >> do
  string "use"
  space
  var <- variableName
  return $ KwUse var

saltyKeywordThrow = debug "saltyKeywordThrow" >> do
  string "throw"
  space
  salty <- saltyParserSingle
  return $ KwThrow salty

saltyKeywordRequire = debug "saltyKeywordRequire" >> do
  string "require"
  space
  salty <- saltyParserSingle
  return $ KwRequire salty

saltyKeywordRequireOnce = debug "saltyKeywordRequireOnce" >> do
  string "require_once"
  space
  salty <- saltyParserSingle
  return $ KwRequireOnce salty

saltyKeywordNamespace = debug "saltyKeywordNamespace" >> do
  string "namespace"
  space
  salty <- saltyParserSingle
  return $ KwNamespace salty

multiAssignVar = debug "multiAssignVar" >> do
  var <- variable
  string ", " <||> string " ="
  return var

multiAssign = debug "multiAssign" >> do
  firstVar <- multiAssignVar
  restVars <- many1 multiAssignVar
  space
  right <- saltyParserSingle
  return $ MultiAssign (firstVar:restVars) right

plusplusAll = debug "plusplusAll" >> do
       plusplusStart
  <||> plusplusEnd
  <||> minusminusStart
  <||> minusminusEnd

plusplusStart = debug "plusplusStart" >> do
  string "++"
  var <- variable
  return $ Operation var PlusEquals (SaltyNumber "1")

plusplusEnd = debug "plusplusEnd" >> do
  var <- variable
  string "++"
  return $ Operation var PlusEquals (SaltyNumber "1")

minusminusStart = debug "minusminusStart" >> do
  string "--"
  var <- variable
  return $ Operation var MinusEquals (SaltyNumber "1")

minusminusEnd = debug "minusminusEnd" >> do
  var <- variable
  string "--"
  return $ Operation var MinusEquals (SaltyNumber "1")

magicConstant strToMatch toReturn = debug "saltyMagicConstant" >> do
  string strToMatch
  return toReturn

saltyMagicConstant = debug "saltyMagicConstant" >> do
  constant <-      magicConstant "__LINE__" MCLINE
              <||> magicConstant "__FILE__" MCFILE
              <||> magicConstant "__DIR__" MCDIR
              <||> magicConstant "__FUNCTION__" MCFUNCTION
              <||> magicConstant "__CLASS__" MCCLASS
              <||> magicConstant "__TRAIT__" MCTRAIT
              <||> magicConstant "__METHOD__" MCMETHOD
              <||> magicConstant "__NAMESPACE__" MCNAMESPACE
  return $ SaltyMagicConstant constant

validRangeArgTypes :: SaltyParser
validRangeArgTypes = debug "validRangeArgTypes" >> do
       constant
  <||> saltyNumber
  <||> functionCall
  <||> attrAccess
  <||> hashLookup
  <||> partialHashLookup
  <||> partialFunctionCall
  <||> partialAttrAccess
  <||> negateSalty
  <||> saltyMagicConstant
  <||> variable

range = debug "range" >> do
  left <- validRangeArgTypes
  string ".."
  right <- validRangeArgTypes
  return $ Range left right

parseError = debug "parseError" >> do
  line <- many1 (noneOf "\n")
  char '\n'
  return $ ParseError line
