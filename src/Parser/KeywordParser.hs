module Parser.KeywordParser where

import Types
import Utils
import Text.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.Combinator

saltySpace = debug "saltySpace" >> do
  space
  return SaltySpace

saltyKeyword followupParser = debug "saltyKeyword" >> do
  optional space
  kw <- (saltyKeywordPreceding followupParser) <||> saltyKeywordSimple
  return $ Keyword kw

saltyKeywordSimple = debug "saltyKeywordSimple" >> do
  kw <-      string "undefined"
         <||> string "break"
  return $ KwSimple kw

saltyKeywordPreceding followupParser = debug "saltyKeywordPreceding" >> do
  kw <-      string "const"
         <||> string "var"
         <||> string "let"
         <||> string "throw"
         <||> string "require"
         <||> string "require_once"
         <||> string "public"
         <||> string "private"
         <||> string "protected"
         <||> string "static"
         <||> string "export"
         <||> string "default"
         <||> string "namespace"
         <||> string "echo"
         <||> string "import *"
         <||> string "import"
         <||> string "as"
         <||> string "use"
         <||> string "from"
         <||> string "2from"
  space
  indentDebugger
  salty <- followupParser
  unindentDebugger
  return $ KwPreceding kw salty
