module Main where

import System.Environment
import System.Directory

import Parser
import Utils

convert :: String -> String -> IO ()
convert infile outfile = do
    contents <- readFile infile
    let out = saltyToPhp contents
    writeFile outfile ("<?php\n" ++ out)

printHelp = do
    putStrLn "Salty is Salmon for PHP."
    putStrLn "Usage: `salty test.salt` (writes to test.php)"
    putStrLn "or `salty input.salt output.php` to write to output.php"

debugFile :: String -> IO ()
debugFile infile = do
    contents <- readFile infile
    putStrLn . saltyToDebugTree $ contents

readFromStdin = do
    contents <- getContents
    putStrLn $ saltyToPhp contents

main = do
  args <- getArgs
  case args of
       ["-h"] -> printHelp
       ["help"] -> printHelp
       ["debug"] -> debugFile "test.salt"
       [inputFile] -> convert inputFile (replace ".salt" ".php" inputFile)
       [inputFile, outputFile] -> convert inputFile outputFile
       [] -> readFromStdin
       _ -> printHelp
