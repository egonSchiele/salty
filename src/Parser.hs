module Parser where

import Types
import Utils
import Formatting
import qualified FormattingJs
import Text.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.Combinator
import ToPhp
import Print
import System.IO.Unsafe (unsafePerformIO)
import System.Environment (lookupEnv)
import qualified Parser.KeywordParser as KeywordParser
import qualified Parser.OperationParser as OperationParser
import qualified Parser.LambdaParser as LambdaParser
import qualified Parser.ArrayParser as ArrayParser
import qualified Parser.VariableParser as VariableParser
import qualified Parser.PrimitiveParser as PrimitiveParser

varNameChars = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_"
extendsNameChars = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_\\."
functionArgsChars = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_&"
typeChars = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_?[]"
-- constChars = oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ_"

nothing = return Nothing

saltyToPhp :: Int -> String -> String
saltyToPhp indentAmt str = case (build str) of
                   Left err -> printError str err
                   Right xs -> saltyToPhp_ indentAmt xs
                   -- Right xs -> checkForErrors str (saltyToPhp_ indentAmt xs)

saltyToJs :: Int -> String -> String
saltyToJs indentAmt str = case (build str) of
                   Left err -> printError str err
                   Right xs -> FormattingJs.saltyToJs_ indentAmt xs
                   -- Right xs -> checkForErrors str (saltyToPhp_ indentAmt xs)


printError :: String -> ParseError -> String
printError inputStr err = print3 "%\n%\n%" affectedLine pointer (show err)
  where line = sourceLine . errorPos $ err
        col  = sourceColumn . errorPos $ err
        affectedLine = (lines inputStr) !! (max 0 (line - 1))
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
       (LambdaFunction args body) -> modifyState (saveIt body)
       salty -> modifyState (saveIt salty)
  return salty

saveIt :: Salty -> SaltyState -> SaltyState
saveIt salty (SaltyState _ scopes i) = SaltyState salty scopes i

saltyParserSingleWithoutNewline :: SaltyParser
saltyParserSingleWithoutNewline = do
      parens
  <|> (try (OperationParser.operation atom saltyParserSingle_))
  <|> saltyBool
  <|> saltyNull
  <|> ArrayParser.emptyObject
  <|> (KeywordParser.saltyKeyword saltyParserSingle_)
  -- these have a double bar b/c saltyKeyword just has too many keywords
  -- to replace them all with `tryString`.
  <||> saltyString
  <|> LambdaParser.lambda lambdaBody
  <|> saltyMagicConstant
  <||> phpVar
  <|> returnStatement
  <|> negateSalty
  <|> saltyComment
  <|> phpComment
  <|> classDefinition
  <|> ArrayParser.array validHashValue
  <|> objectCreation
  <|> purePhp
  <|> ifStatement
  <|> whileStatement
  -- all the number ones
  <|> (    times
      <||> ArrayParser.range
      <||> PrimitiveParser.saltyNumber
      -- minusminusStart is here because it begins with a '-',
      -- just like negative saltyNumbers. So if saltyNumber fails
      -- on "--bar" for example we want to make sure it doesn't eat
      -- a minus sign on the way.
      <||> minusminusStart
      )
  -- things that begin with braces
  <|> (    (ArrayParser.hashTable validHashValue)
      <||> ArrayParser.destructuredHash
      <||> (braces Nothing)
      <||> (ArrayParser.partialHashLookup validFuncArgTypes)
      )
  <|> (try shorthandHtml)
  -- things that begin with a variable name
  <|> (    higherOrderFunctionCall
      <||> function
      <||> functionTypeSignature
      <||> multiAssign
      <||> ArrayParser.indexIntoArray
      <||> shorthandBlock
      <||> functionCall
      <||> attrAccess
      <||> (ArrayParser.arraySlice saltyParserSingle_)
      <||> (ArrayParser.stringIndex saltyParserSingle_)
      <||> (ArrayParser.stringSlice saltyParserSingle_)
      <||> (ArrayParser.hashLookup validFuncArgTypes)
      <||> plusplusAll
      <||> VariableParser.variable
      )
  -- things that begin with a period (.)
  <|> (    partialHigherOrderFunctionCall
      <||> partialFunctionCall
      <||> partialAttrAccess
      )
  <|> (OperationParser.partialOperation saltyParserSingle_)
  <||> html
  <||> emptyLine

validFuncArgTypes :: SaltyParser
validFuncArgTypes = debug "validFuncArgTypes" >> do
       parens
  <||> ArrayParser.emptyObject
  <||> ArrayParser.array validHashValue
  <||> ArrayParser.destructuredHash
  <||> (ArrayParser.hashTable validHashValue)
  <||> (OperationParser.operation atom saltyParserSingle_)
  <||> (OperationParser.partialOperation saltyParserSingle_)
  <||> saltyString
  <||> ArrayParser.range
  <||> ArrayParser.indexIntoArray
  <||> PrimitiveParser.saltyNumber
  <||> functionCall
  <||> attrAccess
  <||> (ArrayParser.arraySlice saltyParserSingle_)
  <||> (ArrayParser.stringIndex saltyParserSingle_)
  <||> (ArrayParser.stringSlice saltyParserSingle_)
  <||> (ArrayParser.hashLookup validFuncArgTypes)
  <||> (ArrayParser.partialHashLookup validFuncArgTypes)
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
  <||> VariableParser.variable
  <?> "a valid function argument type"

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
  <||> ArrayParser.emptyObject
  <||> saltyString
  <||> PrimitiveParser.saltyNumber
  <||> (ArrayParser.hashTable validHashValue)
  <||> ArrayParser.array validHashValue
  <||> functionCall
  <||> (ArrayParser.hashLookup validFuncArgTypes)
  <||> attrAccess
  <||> LambdaParser.lambda lambdaBody
  <||> ArrayParser.destructuredHash
  <||> saltyBool
  <||> saltyNull
  <||> saltyMagicConstant
  <||> negateSalty
  <||> purePhp
  <||> html
  <||> VariableParser.variable
  <?> "a valid hash value"

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
  (DestructuredHash vars) <- ArrayParser.destructuredHash
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
  (name, visibility) <- getVisibility <$> VariableParser.variableName
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
  (name, visibility) <- getVisibility <$> VariableParser.variableName
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
  op <- (OperationParser.operation atom saltyParserSingle_) <||> multiAssign
  unindentDebugger
  optional $ char '\n'
  return (WhereClause op)

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
  (name, visibility) <- getVisibility <$> VariableParser.variableName
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
  name <- VariableParser.variableName
  space
  string "::"
  space
  indentDebugger
  argTypes <- findArgTypes
  unindentDebugger
  return $ FunctionTypeSignature name argTypes

atom = debug "atom" >> do
       parens
  <||> functionCall
  <||> ArrayParser.indexIntoArray
  <||> attrAccess
  <||> (ArrayParser.arraySlice saltyParserSingle_)
  <||> (ArrayParser.stringIndex saltyParserSingle_)
  <||> (ArrayParser.stringSlice saltyParserSingle_)
  <||> (ArrayParser.hashLookup validFuncArgTypes)
  <||> saltyBool
  <||> saltyNull
  <||> saltyMagicConstant
  <||> VariableParser.variable
  <||> saltyString
  <||> PrimitiveParser.saltyNumber
  <?> "an atom"

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
  block <- optionMaybe (try functionBlock)
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

higherOrderFunctionCall = debug "higherOrderFunctionCall" >> do
  indentDebugger
  obj <- ArrayParser.range <||> ArrayParser.indexIntoArray <||> functionCall <||> partialFunctionCall <||> (ArrayParser.array validHashValue) <||> (ArrayParser.arraySlice saltyParserSingle_) <||> (ArrayParser.stringSlice saltyParserSingle_) <||> VariableParser.variable
  unindentDebugger
  char '.'
  funcName <-      (string "map" >> return Map)
              <||> (string "each" >> return Each)
              <||> (string "forEach" >> return Each)
              <||> (string "select" >> return Select)
              <||> (string "filter" >> return Select)
              <||> (string "any" >> return Any)
              <||> (string "some" >> return Any)
              <||> (string "all" >> return All)
              <||> (string "every" >> return All)
  char '('
  indentDebugger
  func <- LambdaParser.lambda lambdaBody
  unindentDebugger
  char ')'
  return $ HigherOrderFunctionCall obj funcName func "$result"

variableAsLambda = debug "variableAsLambda" >> do
  var <- VariableParser.variableName
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
  func <- LambdaParser.lambda lambdaBody
  unindentDebugger
  char ')'
  return $ BackTrack $ HigherOrderFunctionCall obj funcName func "$result"

times = debug "times" >> do
  number <- PrimitiveParser.integer
  char '.'
  string "times"
  char '('
  indentDebugger
  func <- (LambdaParser.lambda lambdaBody) <||> (LambdaParser.lambdaWithoutArgs lambdaBody)
  unindentDebugger
  char ')'
  return $ HigherOrderFunctionCall (Range (SaltyNumber "1") number)  Each func "$result"

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
  htmlNoChildren <||> htmlNoChildrenWithAttrs <||> htmlWithChildren

htmlNoChildren = debug "htmlNoChildren" >> do
  char '<'
  start <- letter
  _tag <- many1 varNameChars
  let tag = start:_tag
  optional space
  string "/>"
  return $ PurePhp $ "<" ++ tag ++ " />"

htmlNoChildrenWithAttrs = debug "htmlNoChildrenWithAttrs" >> do
  char '<'
  start <- letter
  _tag <- many1 varNameChars
  let tag = start:_tag
  space
  attrs <- many1 (noneOf "/")
  string "/>"
  return $ PurePhp $ "<" ++ tag ++ " " ++ attrs ++ "/>"

htmlWithChildren = debug "htmlWithChildren" >> do
  char '<'
  start <- letter
  _tag <- many1 varNameChars
  let tag = start:_tag
  line <- readTill (wrapInSalt (string ("</" ++ tag ++ ">")))
  return $ PurePhp $ "<" ++ tag ++ line ++ "</" ++ tag ++ ">"

functionCall = debug "functionCall" >> do
  indentDebugger
  func <- functionCallOnObject
          <||> functionCallWithoutObject
          <||> functionCallOnObjectWithoutParens
          <||> functionCallWithoutObjectWithoutParens
          <?> "a function call"
  unindentDebugger
  return func

findArgs = debug "findArgs" >> do
  validFuncArgTypes `sepBy` ((string ", ") <||> (string ","))

attrAccess = debug "attrAccess" >> do
  indentDebugger
  obj <- VariableParser.variable
  unindentDebugger
  char '.'
  attrName <- many1 varNameChars
  return $ AttrAccess obj attrName

shorthandBlock = debug "shorthandBlock" >> do
  var <- VariableParser.variable
  indentDebugger
  block <- functionBlock
  unindentDebugger
  return $ FunctionCall (Just var) (Right (SimpleVar "new")) [] (Just block)

htmlVar = debug "htmlVar" >> do
  str <- string "h1" <||> string "h2" <||> string "h3" <||> string "h4" <||> string "h5" <||> string "h6" <||> string "p" <||> string "a" <||> string "img" <||> string "div" <||> string "span" <||> string "html" <||> string "title" <||> string "body" <||> string "b" <||> string "blockquote" <||> string "code" <||> string "em" <||> string "pre" <||> string "small" <||> string "strong" <||> string "sub" <||> string "sup" <||> string "input" <||> string "ul" <||> string "ol" <||> string "li" <||> string "form" <||> string "label" <||> string "button" <||> string "select" <||> string "option" <||> string "fragment" <||> string "table" <||> string "tr" <||> string "td"
  return $ Variable (SimpleVar str) GlobalScope

shorthandHtml = debug "shorthandHtml" >> do
  var <- htmlVar
  space
  val <- saltyString <||> parens
  let arr = case val of
              (Parens salty) -> salty
              _ -> [val]
  return $ FunctionCall (Just var) (Right (SimpleVar "new")) [] (Just (Braces arr))

lambdaBody = (braces Nothing) <||> saltyParserSingle_

functionBlock = debug "functionBlock" >> do
  bracesBlock <||> (LambdaParser.lambdaBlock $ parseTill (wrapInSalt $ string "end"))

bracesBlock = debug "bracesBlock" >> do
  string " do\n"
  indentDebugger
  body <- parseTill (wrapInSalt $ string "end")
  unindentDebugger
  return $ Braces body

functionCallOnObject = debug "functionCallOnObject" >> do
  indentDebugger
  obj <- VariableParser.variable
  char '.'
  funcName <- many1 varNameChars
  char '('
  optional $ char '\n'
  funcArgs <- findArgs
  optional $ char '\n'
  char ')'
  block <- optionMaybe (try functionBlock)
  unindentDebugger
  return $ FunctionCall (Just obj) (Right (SimpleVar funcName)) funcArgs block

parseBuiltInFuncName :: VariableName -> Either BuiltInFunction VariableName
parseBuiltInFuncName (SimpleVar "p") = Left VarDumpShort
parseBuiltInFuncName s = Right s

functionCallWithoutObject = debug "functionCallWithoutObject" >> do
  indentDebugger
  funcName <- VariableParser.variableName
  char '('
  optional $ char '\n'
  funcArgs <- findArgs
  optional $ char '\n'
  char ')'
  unindentDebugger
  block <- optionMaybe (try functionBlock)
  return $ FunctionCall Nothing (parseBuiltInFuncName funcName) funcArgs block

functionCallOnObjectWithoutParens = debug "functionCallOnObjectWithoutParens" >> do
  obj <- VariableParser.variable
  char '.'
  funcName <- many1 varNameChars
  string " . " <||> string " $ "
  indentDebugger
  funcArgs <- functionCallOnObjectWithoutParens <||> functionCallWithoutObjectWithoutParens <||> validFuncArgTypes
  unindentDebugger
  return $ FunctionCall (Just obj) (Right (SimpleVar funcName)) [funcArgs] Nothing

functionCallWithoutObjectWithoutParens = debug "functionCallWithoutObjectWithoutParens" >> do
  funcName <- VariableParser.variableName
  string " . " <||> string " $ "
  indentDebugger
  funcArgs <- functionCallOnObjectWithoutParens <||> functionCallWithoutObjectWithoutParens <||> validFuncArgTypes
  unindentDebugger
  return $ FunctionCall Nothing (parseBuiltInFuncName funcName) [funcArgs] Nothing

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
  name <- VariableParser.classVar
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

objectCreation = debug "objectCreation" >> do
  tryString "new"
  space
  className <- VariableParser.classVar
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

saltyNull = debug "saltyNull" >> do
  s <- tryString "null"
  return SaltyNull

multiAssignVar = debug "multiAssignVar" >> do
  var <- VariableParser.variable
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
  var <- VariableParser.variable
  return $ Operation var PlusEquals (SaltyNumber "1")

plusplusEnd = debug "plusplusEnd" >> do
  var <- VariableParser.variable
  string "++"
  return $ Operation var PlusEquals (SaltyNumber "1")

minusminusStart = debug "minusminusStart" >> do
  string "--"
  var <- VariableParser.variable
  return $ Operation var MinusEquals (SaltyNumber "1")

minusminusEnd = debug "minusminusEnd" >> do
  var <- VariableParser.variable
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

parseError = debug "parseError" >> do
  line <- many1 (noneOf "\n")
  char '\n'
  return $ ParseError line

phpVar = debug "phpVar" >> do
  char '$'
  name <- many1 varNameChars
  return $ PurePhp ('$':name)
