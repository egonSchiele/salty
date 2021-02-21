module Parser.KeywordParser where

import Types
import Utils
import Text.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.Combinator

saltySpace = debug "saltySpace" >> do
  space
  return SaltySpace

maybeToSpace Nothing = ""
maybeToSpace (Just _) = " "

saltyKeyword followupParser = debug "saltyKeyword" >> do
  kw <- (saltyKeywordPreceding followupParser) <||> saltyKeywordSimple
  return $ Keyword kw

saltyKeywordSimple = debug "saltyKeywordSimple" >> do
  sp <- optionMaybe space
  kw <-       string "undefined"
         <||> string "break"
  return $ KwSimple ((maybeToSpace sp) ++ kw)

saltyKeywordPreceding followupParser = debug "saltyKeywordPreceding" >> do
  sp <- optionMaybe space
  kw <-       string "const"
         <||> string "var"
         <||> string "let"
         <||> string "throw"
         <||> string "require_once"
         <||> string "require"
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
  return $ KwPreceding ((maybeToSpace sp) ++ kw) salty
