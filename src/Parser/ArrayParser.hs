module Parser.ArrayParser where

import Types
import Utils
import Text.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.Combinator
import qualified Parser.VariableParser as VariableParser
import qualified Parser.PrimitiveParser as PrimitiveParser

hashKeyChars = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_-'\""
emptyObject = debug "emptyObject" >> do
  tryString "{}" <||> tryString "{ }"
  return $ HashTable []

nothing = return Nothing

array hashValueParser = debug "array" >> do
  char '['
  optional $ char '\n' <||> char ' '
  indentDebugger
  salties <- hashValueParser `sepBy` ((string ", ") <||> (string ",\n") <||> (string ","))
  optional $ char '\n' <||> char ' '
  char ']' <?> "a closing bracket"
  return $ Array salties

-- TODO why is this unused?
destructuredArray = debug "destructuredArray" >> do
  char '['
  optional space
  vars <- VariableParser.variable `sepBy` (string ", " <||> string ",")
  optional space
  char ']'
  return $ Array vars
  -- return $ HashTable (zip (map (SaltyString . getVarName) salties) (map (\s -> Variable s GlobalScope) salties))

stringKey = debug "stringKey" >> do
  key <- many1 hashKeyChars
  return $ SaltyString key

saltyKey = debug "saltyKey" >> do
  char '['
  indentDebugger
  value <- VariableParser.variable
  -- value <- hashLookup <||> saltyBool <||> saltyNull <||> purePhp <||> VariableParser.variable
  unindentDebugger
  char ']' <?> "a closing bracket for a salty key"
  return value

keyValuePair hashValueParser = debug "keyValuePair" >> do
  indentDebugger
  key <- saltyKey <||> stringKey
  unindentDebugger
  optional space
  char ':'
  space
  indentDebugger
  value <- hashValueParser
  unindentDebugger
  char ',' <||> char '}' <||> char '\n' <||> char ' '
  optional (oneOf " \n")
  return (key, value)

hashTable hashValueParser = debug "hashTable" >> do
  char '{'
  optional (oneOf " \n")
  kvPairs <- many1 (keyValuePair hashValueParser)
  optional $ char '}'
  return $ HashTable kvPairs

-- TODO why isn't this used?
-- arrayValue hashValueParser = debug "arrayValue" >> do
--   indentDebugger
--   value <- hashValueParser
--   unindentDebugger
--   char ',' <||> (char ']' <?> "a closing bracket.")
--   optional space
--   return value

-- TODO why isn't this used?
-- hashValue = debug "hashValue" >> do
--   value <- many1 varNameChars
--   char ',' <||> char '}'
--   optional $ oneOf "\n "
--   return value

destructuredHash = debug "destructuredHash" >> do
  string "{ "
  optional $ char '\n'
  vars <- (many1 VariableParser.varNameChars) `sepBy` (string ", " <||> string ",")
  optional $ char '\n'
  string " }"
  return $ DestructuredHash vars
  -- return $ HashTable (zip (map (SaltyString . getVarName) salties) (map (\s -> Variable s GlobalScope) salties))

arraySlice subParser = debug "arraySlice" >> do
  array <- VariableParser.variable
  char '['
  indentDebugger
  start_ <- (Just <$> subParser) <||> nothing
  unindentDebugger
  char ':'
  indentDebugger
  end <- (Just <$> subParser) <||> nothing
  unindentDebugger
  char ']' <?> "a closing bracket for an array slice"
  let start = case start_ of
                Just salty -> salty
                Nothing -> SaltyNumber "0"
  return $ ArraySlice array start end

stringSlice subParser = debug "stringSlice" >> do
  string <- VariableParser.variable
  char '<'
  indentDebugger
  start_ <- (Just <$> subParser) <||> nothing
  unindentDebugger
  char ':'
  indentDebugger
  end <- (Just <$> subParser) <||> nothing
  unindentDebugger
  char '>'
  let start = case start_ of
                Just salty -> salty
                Nothing -> SaltyNumber "0"
  return $ StringSlice string start end

stringIndex subParser = debug "stringIndex" >> do
  string <- VariableParser.variable
  char '<'
  indentDebugger
  index <- subParser
  unindentDebugger
  char '>'
  return $ StringIndex string index

hashLookup keyParser = debug "hashLookup" >> do
       shortHashLookup
  <||> (standardHashLookup keyParser)

shortHashLookup = debug "shortHashLookup" >> do
  char ':'
  hash <- VariableParser.variable
  keys <- many1 $ hashKeyNumber <||> hashKeyString
  return $ foldl (\acc key -> HashLookup acc key) (HashLookup hash (head keys)) (tail keys)

standardHashLookup keyParser = debug "standardHashLookup" >> do
  hash <- VariableParser.variable
  char '['
  indentDebugger
  key <- keyParser
  unindentDebugger
  char ']' <?> "a closing bracket for a hash lookup"
  return $ HashLookup hash key

hashKeyNumber = debug "hashKeyString" >> do
  char '.'
  key <- many1 digit
  return $ SaltyNumber key

hashKeyString = debug "hashKeyString" >> do
  char '.'
  key <- many1 VariableParser.varNameChars
  return $ SaltyString key

partialHashLookup keyParser = debug "partialHashLookup" >> do
  hash <- lastSalty <$> getState
  char '['
  indentDebugger
  key <- keyParser
  unindentDebugger
  char ']' <?> "a closing bracket for a partial hash lookup"
  return $ BackTrack (HashLookup hash key)

indexIntoArray = debug "indexIntoArray" >> do
  var <- VariableParser.variable
  char '.'
  number <- PrimitiveParser.integer
  return $ HashLookup var number

validRangeArgTypes :: SaltyParser
validRangeArgTypes = debug "validRangeArgTypes" >> do
       PrimitiveParser.saltyNumber
  -- <||> functionCall
  -- <||> attrAccess
  -- <||> (ArrayParser.hashLookup validFuncArgTypes)
  -- <||> (ArrayParser.partialHashLookup validFuncArgTypes)
  -- <||> partialFunctionCall
  -- <||> partialAttrAccess
  -- <||> negateSalty
  -- <||> saltyMagicConstant
  <||> VariableParser.variable
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

