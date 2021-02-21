module Parser.PrimitiveParser where

import Types
import Utils
import Text.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.Combinator

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

