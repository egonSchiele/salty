module Parser where

import Types
import Utils
import Formatting
import qualified FormattingJs
import Text.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.Combinator
import Debug.Trace (trace)
import ToPhp
import Print
import System.IO.Unsafe (unsafePerformIO)
import System.Environment (lookupEnv)

tryString :: String -> (Parsec String SaltyState String)
tryString str = try . string $ str

varNameChars = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_"
lambdaVarNameChars = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_ "
classNameChars = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_\\"
extendsNameChars = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_\\."
functionArgsChars = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_&"
hashKeyChars = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_-'\""
typeChars = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_?[]"
-- constChars = oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ_"

data SaltyState = SaltyState {
                      lastSalty :: Salty,
                      stateScopes :: [Scope],
                      debugIndent :: Int
                  }

type SaltyParser = Parsec String SaltyState Salty

debug :: String -> SaltyParser
debug str
  | flag = printDebug str
  | otherwise = return (SaltyString str)
  where flag = unsafePerformIO $ do
          result <- lookupEnv "DEBUG"
          case result of
               Nothing -> return False
               Just _ -> return True

printDebug :: String -> SaltyParser
printDebug str = do
  indent <- debugIndent <$> getState
  parserTrace ((replicate (indent*4) ' ') ++ str)
  return $ SaltyString str

saltyToPhp :: Int -> String -> String
saltyToPhp indentAmt str = case (build str) of
                   Left err -> show err
                   -- Left err -> printError str err
                   Right xs -> saltyToPhp_ indentAmt xs
                   -- Right xs -> checkForErrors str (saltyToPhp_ indentAmt xs)

saltyToJs :: Int -> String -> String
saltyToJs indentAmt str = case (build str) of
                   Left err -> show err
                   -- Left err -> printError str err
                   Right xs -> FormattingJs.saltyToJs_ indentAmt xs
                   -- Right xs -> checkForErrors str (saltyToPhp_ indentAmt xs)


printError :: String -> ParseError -> String
printError inputStr err = print3 "%\n%\n%" affectedLine pointer (show err)
  where line = sourceLine . errorPos $ err
        col  = sourceColumn . errorPos $ err
        affectedLine = (lines inputStr) !! (line - 1)
        pointer = (replicate (col - 1) $ ' ') ++ "^"

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

saltyToDebugTree :: String -> String
saltyToDebugTree str = case (build str) of
                   Left err -> printError str err
                   Right xs -> formatDebug xs

saltyToDebugTreeCheckBackTracks :: String -> String
saltyToDebugTreeCheckBackTracks str = case (build str) of
                   Left err -> printError str err
                   Right xs -> formatDebugStripBackTracks xs


startingState = SaltyState EmptyLine [] 0

-- <* eof operator will return what saltyParser parsed ... otherwise build would return `()`
-- which is what eof returns.
-- idea from https://stackoverflow.com/questions/16209278/parsec-consume-all-input
build :: String -> Either ParseError [Salty]
build str_ = runParser (saltyParser <* eof) startingState "saltyParser" str
  where str = unlines . (map (removeSemicolons . strip)) . lines $ str_

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
  case salty of
       EmptyLine -> return ()
       (Operation left op right) -> modifyState (saveIt right)
       (MultiAssign vars right) -> modifyState (saveIt right)
       salty -> modifyState (saveIt salty)
  return salty

saveIt :: Salty -> SaltyState -> SaltyState
saveIt salty (SaltyState _ scopes i) = SaltyState salty scopes i

indentDebugger :: SaltyParser
indentDebugger = do
  modifyState indentD_
  return Salt

unindentDebugger :: SaltyParser
unindentDebugger = do
  modifyState unindentD_
  return Salt

indentD_ :: SaltyState -> SaltyState
indentD_ (SaltyState prev scope i) = SaltyState prev scope (i+1)

unindentD_ :: SaltyState -> SaltyState
unindentD_ (SaltyState prev scope i) = SaltyState prev scope (i-1)

saltyParserSingleWithoutNewline :: SaltyParser
saltyParserSingleWithoutNewline = do
  parens
  <|> (try operation)
  <|> saltyBool
  <|> saltyNull
  <|> emptyObject
  <|> saltyKeyword
  -- these have a double bar b/c saltyKeyword just has too many keywords
  -- to replace them all with `tryString`.
  <||> saltyString
  <|> lambda
  <|> saltyMagicConstant
  <||> phpVar
  <|> returnStatement
  <|> negateSalty
  <|> saltyComment
  <|> phpComment
  <|> classDefinition
  <|> array
  <|> objectCreation
  <|> purePhp
  <|> ifStatement
  <|> whileStatement
  -- all the number ones
  <|> (    times
      <||> range
      <||> saltyNumber
      -- minusminusStart is here because it begins with a '-',
      -- just like negative saltyNumbers. So if saltyNumber fails
      -- on "--bar" for example we want to make sure it doesn't eat
      -- a minus sign on the way.
      <||> minusminusStart
      )
  -- things that begin with braces
  <|> (    hashTable
      <||> destructuredHash
      <||> (braces Nothing)
      <||> partialHashLookup
      )
  <|> (try shorthandHtml)
  -- things that begin with a variable name
  <|> (    higherOrderFunctionCall
      <||> function
      <||> functionTypeSignature
      <||> multiAssign
      <||> indexIntoArray
      <||> shorthandBlock
      <||> functionCall
      <||> attrAccess
      <||> arraySlice
      <||> stringIndex
      <||> stringSlice
      <||> hashLookup
      <||> plusplusAll
      <||> variable
      )
  -- things that begin with a period (.)
  <|> (    partialHigherOrderFunctionCall
      <||> partialFunctionCall
      <||> partialAttrAccess
      )
  <|> partialOperation
  <||> html
  <||> emptyLine

validFuncArgTypes :: SaltyParser
validFuncArgTypes = debug "validFuncArgTypes" >> do
       parens
  <||> emptyObject
  <||> array
  <||> destructuredHash
  <||> hashTable
  <||> operation
  <||> partialOperation
  <||> saltyString
  <||> range
  <||> indexIntoArray
  <||> saltyNumber
  <||> functionCall
  <||> attrAccess
  <||> arraySlice
  <||> stringIndex
  <||> stringSlice
  <||> hashLookup
  <||> partialHashLookup
  <||> partialFunctionCall
  <||> partialAttrAccess
  <||> negateSalty
  <||> objectCreation
  <||> saltyBool
  <||> saltyNull
  <||> saltyMagicConstant
  <||> phpVar
  <||> purePhp
  <||> html
  <||> variable
  <?> "a valid function argument type"

safeHead [] = Nothing
safeHead (x:xs) = Just x

variable = debug "variable" >> do
  name <- variableName
  scope_ <- (safeHead . stateScopes) <$> getState
  let scope = case scope_ of
       Just scope -> scope
       Nothing -> GlobalScope
  case isConstant (getVarName name) of
       True -> return $ Constant $ Variable name scope
       False -> return $ Variable name scope

variableName = debug "variableName" >> do
        staticVar
  <||>  instanceVar
  <||>  classVar
  <||>  simpleVar
  <?> "a variable"

parens = debug "parens" >> do
  char '('
  indentDebugger
  body <- parseWithoutNewlineTill (wrapInSalt (char ')'))
  unindentDebugger
  modifyState (saveIt (Parens body))
  debug $ "parens done with: " ++ (show body)
  return $ Parens body

validHashValue = debug "validHashValue" >> do
       parens
  <||> emptyObject
  <||> saltyString
  <||> saltyNumber
  <||> hashTable
  <||> array
  <||> functionCall
  <||> hashLookup
  <||> attrAccess
  <||> lambda
  <||> destructuredHash
  <||> saltyBool
  <||> saltyNull
  <||> saltyMagicConstant
  <||> negateSalty
  <||> purePhp
  <||> html
  <||> variable
  <?> "a valid hash value"

stringKey = debug "stringKey" >> do
  key <- many1 hashKeyChars
  return $ SaltyString key

saltyKey = debug "saltyKey" >> do
  char '['
  indentDebugger
  value <- hashLookup <||> saltyBool <||> saltyNull <||> purePhp <||> variable
  unindentDebugger
  char ']' <?> "a closing bracket for a salty key"
  return value

keyValuePair = debug "keyValuePair" >> do
  indentDebugger
  key <- saltyKey <||> stringKey
  unindentDebugger
  optional space
  char ':'
  space
  indentDebugger
  value <- validHashValue
  unindentDebugger
  char ',' <||> char '}' <||> char '\n' <||> char ' '
  optional (oneOf " \n")
  return (key, value)

hashTable = debug "hashTable" >> do
  char '{'
  optional (oneOf " \n")
  kvPairs <- many1 keyValuePair
  optional $ char '}'
  return $ HashTable kvPairs

arrayValue = debug "arrayValue" >> do
  indentDebugger
  value <- validHashValue
  unindentDebugger
  char ',' <||> (char ']' <?> "a closing bracket.")
  optional space
  return value

array = debug "array" >> do
  char '['
  optional $ char '\n' <||> char ' '
  indentDebugger
  salties <- validHashValue `sepBy` ((string ", ") <||> (string ",\n") <||> (string ","))
  optional $ char '\n' <||> char ' '
  char ']' <?> "a closing bracket"
  return $ Array salties

hashValue = debug "hashValue" >> do
  value <- many1 varNameChars
  char ',' <||> char '}'
  optional $ oneOf "\n "
  return value

destructuredHash = debug "destructuredHash" >> do
  string "{ "
  optional $ char '\n'
  vars <- (many1 varNameChars) `sepBy` (string ", " <||> string ",")
  optional $ char '\n'
  string " }"
  return $ DestructuredHash vars
  -- return $ HashTable (zip (map (SaltyString . getVarName) salties) (map (\s -> Variable s GlobalScope) salties))

addScope :: Scope -> SaltyState -> SaltyState
addScope scope (SaltyState prev scopes i) = SaltyState prev (scope:scopes) i

popScope :: SaltyState -> SaltyState
popScope (SaltyState prev (s:scopes) i) = SaltyState prev scopes i

braces :: Maybe Scope -> SaltyParser
braces scope_ = debug "braces" >> do
  char '{'
  optional space
  case scope_ of
       Just scope -> modifyState (addScope scope)
       Nothing -> return ()
  indentDebugger
  body <- saltyParser
  unindentDebugger
  optional space
  case scope_ of
       Just scope -> modifyState popScope
       Nothing -> return ()
  char '}' <?> "a closing brace"
  debug $ "braces done with: " ++ (show body)
  return $ Braces body

function = debug "function" >> do
  multilineFunction <||> guardFunction <||> onelineFunction

varArg = debug "varArg" >> do
  string "..."
  arg <- many1 functionArgsChars
  return $ "..." ++ arg

destructuredArg = debug "destructuredArg" >> do
  (DestructuredHash vars) <- destructuredHash
  return $ "{ " ++ (join ", " vars) ++ " }"

functionArgs = debug "functionArgs" >> many (do
  arg <- varArg <||> destructuredArg <||> many1 functionArgsChars
  space
  return arg)

makeArgNames :: [String] -> [ArgumentName]
makeArgNames [] = []
makeArgNames (arg:rest)
  | arg == "" = [ArgumentName "missing arg" False]
  | head arg == '&' = (ArgumentName (tail arg) True):(makeArgNames rest)
  | otherwise       = (ArgumentName arg False):(makeArgNames rest)

getOrDefaultScope (Just x) = x
getOrDefaultScope Nothing = GlobalScope

saltyNewline = debug "saltyNewline" >> do
  char '\n'
  return Salt

saltyEOF = debug "saltyEOF" >> do
  eof
  return Salt

wrapInSalt parser = debug "wrapInSalt" >> do
  parser
  return Salt

parseTill endParser = debug "parseTill" >> do
  body <- manyTill saltyParserSingle (try (endParser <||> saltyEOF))
  return body

parseWithoutNewlineTill endParser = debug "parseTill" >> do
  body <- manyTill saltyParserSingle_ (try (endParser <||> saltyEOF))
  return body

readTill endParser = debug "readTill" >> do
  body <- manyTill anyChar (try (endParser <||> saltyEOF))
  return body

onelineFunction = debug "onelineFunction" >> do
  prevSalty <- lastSalty <$> getState
  scope <- (getOrDefaultScope . safeHead . stateScopes) <$> getState
  (name, visibility) <- getVisibility <$> variableName
  space
  indentDebugger
  _args <- functionArgs
  unindentDebugger
  let args = makeArgNames _args
  string ":="
  space
  modifyState (addScope FunctionScope)
  indentDebugger
  body <- parseWithoutNewlineTill saltyNewline
  wheres <- many whereClause
  unindentDebugger
  modifyState popScope
  case prevSalty of
       FunctionTypeSignature _ types ->
          return $ Function name (argWithTypes args types) (wheres ++ body) visibility scope
       _ ->
          return $ Function name (map argWithDefaults args) (wheres ++ body) visibility scope

multilineFunction = debug "multilineFunction" >> do
  prevSalty <- lastSalty <$> getState
  scope <- (getOrDefaultScope . safeHead . stateScopes) <$> getState
  (name, visibility) <- getVisibility <$> variableName
  space
  indentDebugger
  _args <- functionArgs
  unindentDebugger
  let args = makeArgNames _args
  string ":="
  space
  indentDebugger
  body <- braces (Just FunctionScope)
  unindentDebugger
  case prevSalty of
     FunctionTypeSignature _ types -> return $ Function name (argWithTypes args types) [body] visibility scope
     _ -> return $ Function name (map argWithDefaults args) [body] visibility scope

otherwiseGuard = do
  string "otherwise" <||> string "_"
  return $ [SaltyString "otherwise"]

-- parseTillEndOfLine = debug "parseTillEndOfLine" >> do
--   body_ <- many1 (noneOf "\n")
--   optional $ char '\n'
--   case (build body_) of
--        Left err -> return $ [SaltyString (show err)]
--        Right body -> return body

toA x = [x]

guard = debug "guard" >> do
  char '|'
  space
  condition <- otherwiseGuard <||> (many1 validFuncArgTypes) <?> "a guard condition"
  optional $ string " -> "
  indentDebugger
  outcome <- (toA <$> braces Nothing) <||> parseWithoutNewlineTill saltyNewline <?> "a guard outcome"
  unindentDebugger
  optional $ char '\n'
  return $ Guard condition outcome

whereClause = debug "whereClause" >> do
  string "where" <||> string "and"
  space
  indentDebugger
  op <- operation <||> multiAssign
  unindentDebugger
  optional $ char '\n'
  return op

saltyGuard = debug "saltyGuard" >> do
       saltyGuardSwitchStatement
  <||> saltyGuardOnly

saltyGuardSwitchStatement = debug "saltyGuardSwitchStatement" >> do
  string "guard" <||> string "$$"
  char '('
  indentDebugger
  val <- validFuncArgTypes
  unindentDebugger
  char ')'
  char '\n'
  indentDebugger
  guards <- many1 guard
  unindentDebugger
  return $ SaltyGuard (Just val) guards

saltyGuardOnly = debug "saltyGuardOnly" >> do
  string "guard\n" <||> string "$$\n"
  indentDebugger
  guards <- many1 guard
  unindentDebugger
  return $ SaltyGuard Nothing guards

guardFunction = debug "guardFunction" >> do
  prevSalty <- lastSalty <$> getState
  scope <- (getOrDefaultScope . safeHead . stateScopes) <$> getState
  (name, visibility) <- getVisibility <$> variableName
  space
  args <- makeArgNames <$> functionArgs
  string ":= "
  modifyState (addScope FunctionScope)
  indentDebugger
  guards <- saltyGuard
  wheres <- many whereClause
  unindentDebugger
  let body = wheres ++ [guards]
  modifyState popScope
  case prevSalty of
     FunctionTypeSignature _ types -> return $ Function name (argWithTypes args types) body visibility scope
     _ -> return $ Function name (map argWithDefaults args) body visibility scope

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
  indentDebugger
  argTypes <- findArgTypes
  unindentDebugger
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
  <?> "an operator"

atom = debug "atom" >> do
       parens
  <||> functionCall
  <||> indexIntoArray
  <||> attrAccess
  <||> arraySlice
  <||> stringIndex
  <||> stringSlice
  <||> hashLookup
  <||> saltyBool
  <||> saltyNull
  <||> saltyMagicConstant
  <||> variable
  <||> saltyString
  <||> saltyNumber
  <?> "an atom"

operation = debug "operation" >> do
  indentDebugger
  left <- atom
  unindentDebugger
  space
  op <- operator
  space
  indentDebugger
  right <- saltyParserSingle_
  unindentDebugger
  return $ Operation left op right

partialOperation = debug "partialOperation" >> do
  leftHandSide <- lastSalty <$> getState
  space
  op <- operator
  space
  indentDebugger
  right <- saltyParserSingle_
  unindentDebugger
  return $ BackTrack (Operation leftHandSide op right)

partialAttrAccess = debug "partialAttrAccess" >> do
  leftHandSide <- lastSalty <$> getState
  char '.'
  attrName <- many1 varNameChars
  return $ BackTrack (AttrAccess leftHandSide attrName)

partialFunctionCall = debug "partialFunctionCall" >> do
  leftHandSide <- lastSalty <$> getState

  char '.'
  funcName <- many1 varNameChars
  char '('
  indentDebugger
  funcArgs <- findArgs
  unindentDebugger
  char ')'

  indentDebugger
  block <- optionMaybe functionBlock
  unindentDebugger

  return $ case leftHandSide of
             Operation l op r -> BackTrack $ Operation l op (FunctionCall (Just r) (Right (SimpleVar funcName)) funcArgs block)
             _ -> BackTrack $ FunctionCall (Just leftHandSide) (Right (SimpleVar funcName)) funcArgs block

negateSalty = debug "negateSalty" >> do
  char '!'
  indentDebugger
  s <- saltyParserSingle_
  unindentDebugger
  return $ Negate s

emptyLine = debug "emptyLine" >> do
  string "\n"
  return EmptyLine

saltyString = debug "saltyString" >> do
  quoteType <- oneOf "'\""
  str <- many $ noneOf [quoteType]
  char quoteType
  return $ SaltyString str

saltyNumber = debug "saltyNumber" >> do
       decimal <||> negativeDecimal
  <||> integer <||> negativeInteger
  <?> "a decimal or integer"

negativeInteger = debug "negativeInteger" >> do
  char '-'
  number <- many1 (oneOf "1234567890")
  return $ SaltyNumber ('-':number)

integer = debug "integer" >> do
  number <- many1 (oneOf "1234567890")
  return $ SaltyNumber number

negativeDecimal = debug "negativeDecimal" >> do
  char '-'
  number <- many1 (oneOf "1234567890")
  char '.'
  decimal <- many1 (oneOf "1234567890")
  return $ SaltyNumber (('-':number) ++ "." ++ decimal)

decimal = debug "decimal" >> do
  number <- many1 (oneOf "1234567890")
  char '.'
  decimal <- many1 (oneOf "1234567890")
  return $ SaltyNumber (number ++ "." ++ decimal)

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

classVar = debug "classVar" >> do
  classicClassVar <||> selfClassVar

selfClassVar = debug "selfClassVar" >> do
  string "self"
  return $ ClassVar "self"

classicClassVar = debug "classicClassVar" >> do
  start <- upper
  variable <- many1 classNameChars
  return $ ClassVar (start:variable)

higherOrderFunctionCall = debug "higherOrderFunctionCall" >> do
  indentDebugger
  obj <- range <||> indexIntoArray <||> functionCall <||> partialFunctionCall <||> array <||> arraySlice <||> stringSlice <||> variable
  unindentDebugger
  char '.'
  funcName <-      (string "map" >> return Map)
              <||> (string "each" >> return Each)
              <||> (string "select" >> return Select)
              <||> (string "filter" >> return Select)
              <||> (string "any" >> return Any)
              <||> (string "all" >> return All)
  char '('
  indentDebugger
  func <- lambda
  unindentDebugger
  char ')'
  return $ HigherOrderFunctionCall obj funcName func "$result"

variableAsLambda = debug "variableAsLambda" >> do
  var <- variableName
  return $ LambdaFunction ["x"] (FunctionCall Nothing (Right var) [Variable (SimpleVar "x") GlobalScope] Nothing)

partialHigherOrderFunctionCall = debug "partialHigherOrderFunctionCall" >> do
  obj <- lastSalty <$> getState

  char '.'
  funcName <-      (string "map" >> return Map)
              <||> (string "each" >> return Each)
              <||> (string "select" >> return Select)
              <||> (string "filter" >> return Select)
              <||> (string "any" >> return Any)
              <||> (string "all" >> return All)
  char '('
  indentDebugger
  func <- lambda
  unindentDebugger
  char ')'
  return $ BackTrack $ HigherOrderFunctionCall obj funcName func "$result"

times = debug "times" >> do
  number <- integer
  char '.'
  string "times"
  char '('
  indentDebugger
  func <- lambda <||> lambdaWithoutArgs
  unindentDebugger
  char ')'
  return $ HigherOrderFunctionCall (Range (SaltyNumber "1") number)  Each func "$result"

lambda = debug "lambda" >> do
  char '\\'
  args <-  many1 lambdaVarNameChars
  string "-> "
  indentDebugger
  body <- (braces Nothing) <||> saltyParserSingle_ <?> "a lambda function body"
  unindentDebugger
  return $ LambdaFunction (words args) body

lambdaWithoutArgs = debug "lambdaWithoutArgs" >> do
  indentDebugger
  body <- (braces Nothing) <||> saltyParserSingle_ <?> "a lambda function (without args) body"
  unindentDebugger
  return $ LambdaFunction [] body

returnStatement = debug "returnStatement" >> do
  tryString "return "
  indentDebugger
  salty <- many1 saltyParserSingle_
  unindentDebugger
  optional $ char '\n'
  return $ ReturnStatement (Braces salty)

saltyComment = do
  char '#' <?> "a salty comment"
  line <- many1 $ noneOf "\n"
  string "\n"
  return $ SaltyComment line

phpComment = (do
  tryString "//" <?> "a php comment"
  line <- many1 $ noneOf "\n"
  string "\n"
  return $ PhpComment line) <?> "a php comment"

purePhp = debug "purePhp" >> do
  string "`"
  line <- many1 $ noneOf "`"
  string "`"
  return $ PurePhp line

html = debug "html" >> do
  htmlNoChildren <||> htmlWithChildren

htmlNoChildren = debug "htmlNoChildren" >> do
  char '<'
  start <- letter
  _tag <- many1 varNameChars
  let tag = start:_tag
  optional space
  string "/>"
  return $ PurePhp $ "<" ++ tag ++ " />"

htmlWithChildren = debug "htmlWithChildren" >> do
  char '<'
  start <- letter
  _tag <- many1 varNameChars
  let tag = start:_tag
  line <- readTill (wrapInSalt (string ("</" ++ tag ++ ">")))
  return $ PurePhp $ "<" ++ tag ++ line ++ "</" ++ tag ++ ">"

functionCall = debug "functionCall" >> do
       functionCallOnObject
  <||> functionCallWithoutObject
  <||> functionCallOnObjectWithoutParens
  <||> functionCallWithoutObjectWithoutParens
  <?> "a function call"

findArgs = debug "findArgs" >> do
  validFuncArgTypes `sepBy` ((string ", ") <||> (string ","))

attrAccess = debug "attrAccess" >> do
  obj <- variable
  char '.'
  attrName <- many1 varNameChars
  return $ AttrAccess obj attrName

shorthandBlock = debug "shorthandBlock" >> do
  var <- variable
  indentDebugger
  block <- functionBlock
  unindentDebugger
  return $ FunctionCall (Just var) (Right (SimpleVar "new")) [] (Just block)

htmlVar = debug "htmlVar" >> do
  str <- string "h1" <||> string "h2" <||> string "h3" <||> string "h4" <||> string "h5" <||> string "h6" <||> string "p" <||> string "a" <||> string "img" <||> string "div" <||> string "span" <||> string "html" <||> string "title" <||> string "body" <||> string "b" <||> string "blockquote" <||> string "code" <||> string "em" <||> string "pre" <||> string "small" <||> string "strong" <||> string "sub" <||> string "sup" <||> string "input" <||> string "ul" <||> string "ol" <||> string "li" <||> string "form" <||> string "label" <||> string "button" <||> string "select" <||> string "option" <||> string "fragment"
  return $ Variable (SimpleVar str) GlobalScope

shorthandHtml = debug "shorthandHtml" >> do
  var <- htmlVar
  space
  (SaltyString str) <- saltyString
  return $ FunctionCall (Just var) (Right (SimpleVar "new")) [] (Just (Braces [PurePhp str]))

functionBlock = debug "functionBlock" >> do
  bracesBlock <||> lambdaBlock

bracesBlock = debug "bracesBlock" >> do
  string " do\n"
  indentDebugger
  body <- parseTill (wrapInSalt $ string "end")
  unindentDebugger
  return $ Braces body

lambdaBlock = debug "lambdaBlock" >> do
  string " do \\"
  args <-  many1 lambdaVarNameChars
  string "->\n"
  indentDebugger
  body <- parseTill (wrapInSalt $ string "end")
  unindentDebugger
  return $ LambdaFunction (words args) (Braces body)

functionCallOnObject = debug "functionCallOnObject" >> do
  obj <- variable
  char '.'
  funcName <- many1 varNameChars
  char '('
  indentDebugger
  funcArgs <- findArgs
  unindentDebugger
  char ')'
  indentDebugger
  block <- optionMaybe functionBlock
  unindentDebugger
  return $ FunctionCall (Just obj) (Right (SimpleVar funcName)) funcArgs block

parseBuiltInFuncName :: VariableName -> Either BuiltInFunction VariableName
parseBuiltInFuncName (SimpleVar "p") = Left VarDumpShort
parseBuiltInFuncName s = Right s

functionCallWithoutObject = debug "functionCallWithoutObject" >> do
  funcName <- variableName
  char '('
  indentDebugger
  funcArgs <- findArgs
  unindentDebugger
  char ')'
  -- TO FIX: adding the ability to have a block here causes some tests to fail.
  -- fix and then uncomment
  -- block <- optionMaybe functionBlock
  -- return $ FunctionCall Nothing (parseBuiltInFuncName funcName) funcArgs block
  return $ FunctionCall Nothing (parseBuiltInFuncName funcName) funcArgs Nothing

functionCallOnObjectWithoutParens = debug "functionCallOnObjectWithoutParens" >> do
  obj <- variable
  char '.'
  funcName <- many1 varNameChars
  string " . " <||> string " $ "
  indentDebugger
  funcArgs <- functionCallOnObjectWithoutParens <||> functionCallWithoutObjectWithoutParens <||> validFuncArgTypes
  unindentDebugger
  return $ FunctionCall (Just obj) (Right (SimpleVar funcName)) [funcArgs] Nothing

functionCallWithoutObjectWithoutParens = debug "functionCallWithoutObjectWithoutParens" >> do
  funcName <- variableName
  string " . " <||> string " $ "
  indentDebugger
  funcArgs <- functionCallOnObjectWithoutParens <||> functionCallWithoutObjectWithoutParens <||> validFuncArgTypes
  unindentDebugger
  return $ FunctionCall Nothing (parseBuiltInFuncName funcName) [funcArgs] Nothing

arraySlice = debug "arraySlice" >> do
  array <- variable
  char '['
  indentDebugger
  start_ <- (Just <$> saltyParserSingle_) <||> nothing
  unindentDebugger
  char ':'
  indentDebugger
  end <- (Just <$> saltyParserSingle_) <||> nothing
  unindentDebugger
  char ']' <?> "a closing bracket for an array slice"
  let start = case start_ of
                Just salty -> salty
                Nothing -> SaltyNumber "0"
  return $ ArraySlice array start end

stringSlice = debug "stringSlice" >> do
  string <- variable
  char '<'
  indentDebugger
  start_ <- (Just <$> saltyParserSingle_) <||> nothing
  unindentDebugger
  char ':'
  indentDebugger
  end <- (Just <$> saltyParserSingle_) <||> nothing
  unindentDebugger
  char '>'
  let start = case start_ of
                Just salty -> salty
                Nothing -> SaltyNumber "0"
  return $ StringSlice string start end

stringIndex = debug "stringIndex" >> do
  string <- variable
  char '<'
  indentDebugger
  index <- saltyParserSingle_
  unindentDebugger
  char '>'
  return $ StringIndex string index

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
  indentDebugger
  key <- validFuncArgTypes
  unindentDebugger
  char ']' <?> "a closing bracket for a hash lookup"
  return $ HashLookup hash key

partialHashLookup = debug "partialHashLookup" >> do
  hash <- lastSalty <$> getState
  char '['
  indentDebugger
  key <- validFuncArgTypes
  unindentDebugger
  char ']' <?> "a closing bracket for a partial hash lookup"
  return $ BackTrack (HashLookup hash key)

ifStatement = debug "ifStatement" >> do
       ifWithElse
  <||> ifWithoutElse
  <?> "an if statement"

ifWithElse = debug "ifWithElse" >> do
  tryString "if"
  space
  indentDebugger
  condition <- parseTill (wrapInSalt $ string " then ")
  unindentDebugger
  indentDebugger
  thenFork <- saltyParserSingle_
  unindentDebugger
  space
  string "else"
  space
  indentDebugger
  elseFork <- saltyParserSingle_
  unindentDebugger
  return $ If condition thenFork (Just elseFork)

ifWithoutElse = debug "ifWithoutElse" >> do
  tryString "if"
  space
  indentDebugger
  condition <- parseTill (wrapInSalt $ string " then ")
  thenFork <- saltyParserSingle_
  unindentDebugger
  return $ If condition thenFork Nothing

whileStatement = debug "whileStatement" >> do
  tryString "while"
  space
  indentDebugger
  condition <- saltyParserSingle_
  unindentDebugger
  space
  indentDebugger
  body <- braces Nothing
  unindentDebugger
  return $ While condition body

whereStatement = debug "whereStatement" >> do
  string "where"
  optional space
  modifyState (addScope ClassScope)
  indentDebugger
  body <- saltyParser
  unindentDebugger
  optional space
  modifyState popScope
  debug $ "whereStatement done with: " ++ (show body)
  return $ Braces body

classDefinition = debug "classDefinition" >> do
  tryString "class"
  space
  name <- classVar
  space
  extendsName <- classDefExtends <||> nothing
  implementsName <- classDefImplements <||> nothing
  optional space
  indentDebugger
  body <- (braces (Just ClassScope) <||> whereStatement) <?> "a class body"
  unindentDebugger
  return $ Class name extendsName implementsName body

classDefExtends = debug "classDefExtends" >> do
  string "extends"
  space
  extendsName <- many1 extendsNameChars
  space
  return $ Just (ClassVar extendsName)

classDefImplements = debug "classDefImplements" >> do
  string "implements"
  space
  implementsName <- many1 extendsNameChars
  space
  return $ Just (ClassVar implementsName)

nothing = return Nothing

objectCreation = debug "objectCreation" >> do
  tryString "new"
  space
  className <- classVar
  char '('
  indentDebugger
  constructorArgs <- findArgs
  unindentDebugger
  char ')'
  return $ New className constructorArgs

saltyBool = debug "saltyBool" >> (saltyTrue <|> saltyFalse)

saltyTrue = debug "saltyTrue" >> do
  s <- tryString "true"
  return $ SaltyBool TRUE

saltyFalse = debug "saltyFalse" >> do
  s <- tryString "false"
  return $ SaltyBool FALSE

emptyObject = debug "emptyObject" >> do
  tryString "{}" <||> tryString "{ }"
  return $ HashTable []

saltyNull = debug "saltyNull" >> do
  s <- tryString "null"
  return SaltyNull

saltyKeyword = debug "saltyKeyword" >> do
  phpKeyword <-      saltyKeywordUse
                <||> saltyKeywordThrow
                <||> saltyKeywordRequire
                <||> saltyKeywordRequireOnce
                <||> saltyKeywordImport
                <||> saltyKeywordVarDeclaration
                <||> saltyKeywordPublic
                <||> saltyKeywordPrivate
                <||> saltyKeywordProtected
                <||> saltyKeywordStatic
                <||> saltyKeywordExport
                <||> saltyKeywordDefault
                <||> saltyKeywordNamespace
                <||> saltyKeywordEcho
                <||> saltyKeywordBreak
                <||> saltyKeywordUndefined
  return $ Keyword phpKeyword

saltyKeywordUse = debug "saltyKeywordUse" >> do
  saltyKeywordUseAs <||> saltyKeywordUseOnly

saltyKeywordUseAs = debug "saltyKeywordUseAs" >> do
  string "use"
  space
  var <- variableName
  space
  string "as"
  space
  varAs <- variableName
  return $ KwUse var (Just varAs)

saltyKeywordUseOnly = debug "saltyKeywordUseOnly" >> do
  string "use"
  space
  var <- variableName
  return $ KwUse var Nothing

saltyKeywordThrow = debug "saltyKeywordThrow" >> do
  string "throw"
  space
  salty <- saltyParserSingle_
  return $ KwThrow salty

saltyKeywordRequire = debug "saltyKeywordRequire" >> do
  string "require"
  space
  indentDebugger
  salty <- saltyParserSingle_
  unindentDebugger
  return $ KwRequire salty

saltyKeywordRequireOnce = debug "saltyKeywordRequireOnce" >> do
  string "require_once"
  space
  indentDebugger
  salty <- saltyParserSingle_
  unindentDebugger
  return $ KwRequireOnce salty

saltyKeywordImport = debug "saltyKeywordImport" >> do
  string "import"
  space
  salty <- many1 $ noneOf "\n"
  return $ KwImport $ PurePhp salty

saltyKeywordVarDeclaration = debug "saltyKeywordVarDeclaration" >> do
  typ <- string "const" <||> string "var" <||> string "let"
  space
  indentDebugger
  name <- (PurePhp <$> many1 varNameChars) <||> destructuredHash
  unindentDebugger
  return $ KwVarDeclaration typ name

saltyKeywordPublic = debug "saltyKeywordPublic" >> do
  string "public"
  space
  indentDebugger
  salty <- saltyParserSingle_
  unindentDebugger
  return $ KwPublic salty

saltyKeywordPrivate = debug "saltyKeywordPrivate" >> do
  string "private"
  space
  indentDebugger
  salty <- saltyParserSingle_
  unindentDebugger
  return $ KwPrivate salty

saltyKeywordProtected = debug "saltyKeywordProtected" >> do
  string "protected"
  space
  indentDebugger
  salty <- saltyParserSingle_
  unindentDebugger
  return $ KwProtected salty

saltyKeywordStatic = debug "saltyKeywordStatic" >> do
  string "static"
  space
  indentDebugger
  salty <- saltyParserSingle_
  unindentDebugger
  return $ KwStatic salty

saltyKeywordExport = debug "saltyKeywordExport" >> do
  string "export"
  space
  indentDebugger
  salty <- saltyParserSingle_
  unindentDebugger
  return $ KwExport salty

saltyKeywordDefault = debug "saltyKeywordDefault" >> do
  string "default"
  space
  indentDebugger
  salty <- saltyParserSingle_
  unindentDebugger
  return $ KwDefault salty

saltyKeywordNamespace = debug "saltyKeywordNamespace" >> do
  string "namespace"
  space
  indentDebugger
  salty <- saltyParserSingle_
  unindentDebugger
  return $ KwNamespace salty

saltyKeywordEcho = debug "saltyKeywordEcho" >> do
  string "echo"
  space
  indentDebugger
  salty <- saltyParserSingle_
  unindentDebugger
  return $ KwEcho salty

saltyKeywordBreak = debug "saltyKeywordBreak" >> do
  string "break"
  return KwBreak

saltyKeywordUndefined = debug "saltyKeywordUndefined" >> do
  string "undefined"
  return KwUndefined

multiAssignVar = debug "multiAssignVar" >> do
  var <- variable
  string ", " <||> string " ="
  return var

multiAssign = debug "multiAssign" >> do
  firstVar <- multiAssignVar
  restVars <- many1 multiAssignVar
  space
  indentDebugger
  right <- saltyParserSingle_
  unindentDebugger
  return $ MultiAssign (firstVar:restVars) right

plusplusAll = debug "plusplusAll" >> do
       plusplusStart
  <||> plusplusEnd
  <||> minusminusStart
  <||> minusminusEnd
  <?> "++ or -- statement"

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
       saltyNumber
  <||> functionCall
  <||> attrAccess
  <||> hashLookup
  <||> partialHashLookup
  <||> partialFunctionCall
  <||> partialAttrAccess
  <||> negateSalty
  <||> saltyMagicConstant
  <||> variable
  <?> "a valid range argument type"

range = debug "range" >> do
  indentDebugger
  left <- validRangeArgTypes
  unindentDebugger
  string ".."
  indentDebugger
  right <- validRangeArgTypes
  unindentDebugger
  return $ Range left right

parseError = debug "parseError" >> do
  line <- many1 (noneOf "\n")
  char '\n'
  return $ ParseError line

phpVar = debug "phpVar" >> do
  char '$'
  name <- many1 varNameChars
  return $ PurePhp ('$':name)

indexIntoArray = debug "indexIntoArray" >> do
  var <- variable
  char '.'
  number <- integer
  return $ HashLookup var number
