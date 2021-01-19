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

debugFromStdin = do
    contents <- getContents
    putStrLn . saltyToDebugTree $ contents

main = do
  args <- getArgs
  case args of
       ["-h"] -> printHelp
       ["help"] -> printHelp
       ["debug", inputFile] -> debugFile inputFile
       ["-d", inputFile] -> debugFile inputFile
       ["debug"] -> debugFromStdin
       ["-d"] -> debugFromStdin
       [inputFile] -> convert inputFile
       [] -> readFromStdin
       _ -> printHelp
