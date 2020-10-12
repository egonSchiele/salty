module Lib where

import Text.Parsec (parse)
import Text.ParserCombinators.Parsec.Char (anyChar)
import Text.Parsec.Combinator (many1)
import Text.ParserCombinators.Parsec.Error(messageString)


build :: String -> String
build str = case (parse numbers str "hi") of
                 Left err -> show err
                 Right xs -> xs

numbers = many1 anyChar
