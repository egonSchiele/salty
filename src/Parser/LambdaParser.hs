module Parser.LambdaParser where

import Types
import Utils
import Text.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.Combinator

lambdaVarNameChars = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_ "

lambda bodyParser = debug "lambda" >> do
  char '\\'
  args <-  many1 lambdaVarNameChars
  string "-> "
  indentDebugger
  body <- bodyParser
  unindentDebugger
  return $ LambdaFunction (words args) body

lambdaWithoutArgs bodyParser = debug "lambdaWithoutArgs" >> do
  indentDebugger
  body <- bodyParser
  unindentDebugger
  return $ LambdaFunction [] body

lambdaBlock bodyParser = debug "lambdaBlock" >> do
  string " do \\"
  args <-  many1 lambdaVarNameChars
  string "->\n"
  indentDebugger
  body <- bodyParser
  unindentDebugger
  return $ LambdaFunction (words args) (Braces body)

