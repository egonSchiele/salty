module Main where

import System.Environment
import Control.Applicative
import Text.Printf
import Control.Monad
import Control.Monad.State
import System.Directory

import Text.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.Combinator

import Lib

convert :: String -> String -> IO ()
convert infile outfile = do
    contents <- readFile infile
    let out = saltyToPhp contents
    liftIO $ writeFile outfile ("<?php\n" ++ out)

printHelp = do
    putStrLn "Salty is Salmon for PHP."
    putStrLn "Usage: `salty test.salt` (writes to test.php)"
    putStrLn "or `salty input.salt output.php` to write to output.php"

debugFile :: String -> IO ()
debugFile infile = do
    contents <- readFile infile
    putStrLn . saltyToDebugTree $ contents

-- foo = do
--   name <- many1 letter
--   oneOf " ,;)\n"
--   return name

-- bar = do
--   variable <- letter `manyTill` (lookAhead . try $ (oneOf " .),\n;"))
--   return variable

-- test = do
--   parseTest bar "foo a"

-- main = test
main = do
  args <- getArgs
  case args of
       ["debug"] -> debugFile "test.salt"
       _ -> convert "test.salt" "test.php"
