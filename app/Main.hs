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
    contents <- lines <$> readFile infile
    let out = map build contents
    liftIO $ writeFile outfile (unlines out)

printHelp = do
    putStrLn "Salty is Salmon for PHP."
    putStrLn "Usage: `salty test.sl` (writes to test.php)"
    putStrLn "or `salty input.sl output.php` to write to output.php"


main = do
  convert "test.sl" "test.php"
