module Main where

import System.Environment
import System.Directory

import Parser
import Utils

convert :: String -> IO ()
convert infile = do
    contents <- readFile infile
    let out = saltyToPhp contents
    putStrLn ("<?php\n" ++ out)

printHelp = do
    putStrLn "Salty is Salmon for PHP."
    putStrLn "Usage: `salty test.salt` prints to stdout"
    putStrLn "Usage: `salty` reads from stdin and prints to stdout"
    putStrLn "Usage: `salty debug <filename>` reads file and prints tree to stdout"

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
       ["debug", inputFile] -> debugFile inputFile
       [inputFile] -> convert inputFile
       [] -> readFromStdin
       _ -> printHelp
