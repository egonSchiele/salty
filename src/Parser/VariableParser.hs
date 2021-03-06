module Parser.VariableParser where

import Types
import Utils
import Text.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.Combinator

varNameChars = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_"
classNameChars = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_\\"

getVarName :: VariableName -> String
getVarName (InstanceVar str) = str
getVarName (StaticVar str) = str
getVarName (ClassVar str) = str
getVarName (SimpleVar str) = str

variable = debug "variable" >> do
  name <- variableName
  scope_ <- (safeHead . stateScopes) <$> getState
  let scope = case scope_ of
       Just scope -> scope
       Nothing -> GlobalScope
  case isConstant (getVarName name) of
       True -> return $ Constant $ Variable name scope
       False -> return $ Variable name scope

-- variableName = debug "variableName" >> do
variableName = do
        staticVar
  <||>  instanceVar
  <||>  classVar
  <||>  simpleVar
  <?> "a variable"

-- @foo
-- instanceVar = debug "instanceVar" >> do
instanceVar = do
  char '@'
  variable <- many1 varNameChars
  return $ InstanceVar variable

-- @@foo
-- staticVar = debug "staticVar" >> do
staticVar = do
  string "@@"
  variable <- many1 varNameChars
  return $ StaticVar variable

-- foo
-- simpleVar = debug "simpleVar" >> do
simpleVar = do
  first <- (letter <||> char '_')
  rest <- many varNameChars
  return $ SimpleVar (first:rest)

-- classVar = debug "classVar" >> do
classVar = do
  classicClassVar <||> selfClassVar

-- selfClassVar = debug "selfClassVar" >> do
selfClassVar = do
  string "self"
  return $ ClassVar "self"

className = do
  start <- upper
  variable <- many1 classNameChars
  return (start:variable)

-- classicClassVar = debug "classicClassVar" >> do
classicClassVar = do
  parts <- className `sepBy1` (char '.')
  return $ ClassVar (join "." parts)

