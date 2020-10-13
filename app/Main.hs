module Main where

import System.Environment
import Control.Applicative
import Text.Printf
import Control.Monad
import Control.Monad.State
import System.Directory

import Lib

convert :: String -> String -> IO ()
convert infile outfile = do
    contents <- readFile infile
    let out = saltyToPhp contents
    liftIO $ writeFile outfile out

printHelp = do
    putStrLn "Salty is Salmon for PHP."
    putStrLn "Usage: `salty test.sl` (writes to test.php)"
    putStrLn "or `salty input.sl output.php` to write to output.php"

debug :: String -> IO ()
debug infile = do
    contents <- readFile infile
    putStrLn . saltyToDebugTree $ contents

main = do
  args <- getArgs
  case args of
       ["debug"] -> debug "test.sl"
       _ -> convert "test.sl" "test.php"
