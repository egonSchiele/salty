module Main where

import System.Environment
import System.Directory

import Parser
import ParserPhp (phpToPhp)
import Utils

getIndentAmt :: String -> Int
getIndentAmt contents = floor $ (countLeadingSpaces contents) / 4

countLeadingSpaces [] = 0
countLeadingSpaces (' ':xs) = 1 + (countLeadingSpaces xs)
countLeadingSpaces _ = 0

convert :: String -> IO ()
convert infile = do
    contents <- readFile infile
    let out = saltyToPhp (getIndentAmt contents) contents
    putStrLn ("<?php\n" ++ out)

convertToFile infile outfile = do
    contents <- readFile infile
    let out = saltyToPhp (getIndentAmt contents) contents
    writeFile outfile ("<?php\n" ++ out)

convertToPhp infile = do
    contents <- readFile infile
    let out = phpToPhp (getIndentAmt contents) contents
    putStrLn out

printHelp = do
    putStrLn "Usage: `salty test.salt` prints to stdout"
    putStrLn "Usage: `salty` reads from stdin and prints to stdout"
    putStrLn "Usage: `salty debug <filename>` reads file and prints tree to stdout"

debugFile :: String -> IO ()
debugFile infile = do
    contents <- readFile infile
    putStrLn . saltyToDebugTree $ contents

findErrorInFile infile = do
  contents <- readFile infile
  putStrLn "error in:"
  putStrLn . findErrorLine $ contents

findErrorInStdin = do
  contents <- getContents
  putStrLn "error in:"
  putStrLn . findErrorLine $ contents

readFromStdin = do
    contents <- getContents
    putStrLn $ saltyToPhp (getIndentAmt contents) contents

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
       ["-e"] -> findErrorInStdin
       ["-f", inputFile] -> convertToFile inputFile (replace ".salt" ".php" inputFile)
       ["-e", inputFile] -> findErrorInFile inputFile
       [inputFile] -> convert inputFile
       [] -> readFromStdin
       ["-p", inputFile] -> convertToPhp inputFile
       _ -> printHelp
