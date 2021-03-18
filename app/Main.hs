module Main where

import System.Environment
import System.Directory

import Parser
import Utils
import Print (print2)

getIndentAmt :: String -> Int
getIndentAmt contents = floor $ (countLeadingSpaces contents) / 4

countLeadingSpaces [] = 0
countLeadingSpaces (' ':xs) = 1 + (countLeadingSpaces xs)
countLeadingSpaces _ = 0

convertPhpPrintToStdout :: String -> IO ()
convertPhpPrintToStdout infile = do
    contents <- readFile infile
    let out = saltyToPhp (getIndentAmt contents) contents
    putStrLn ("<?php\n" ++ out)

convertToPhpFile infile outfile = do
    contents <- readFile infile
    let out = saltyToPhp (getIndentAmt contents) contents
    writeFile outfile ("<?php\n" ++ out)

convertToJsFile infile outfile = do
    contents <- readFile infile
    let out = saltyToJs (getIndentAmt contents) contents
    writeFile outfile out

printHelp = do
    putStrLn "Usage: `salty test.salt` prints to stdout"
    putStrLn "Usage: `salty` reads from stdin and prints to stdout"
    putStrLn "Usage: `salty debug <filename>` reads file and prints tree to stdout"

debugFile :: String -> IO ()
debugFile infile = do
    contents <- readFile infile
    putStrLn . saltyToDebugTree $ contents

debugFileCheckBackTracks :: String -> IO ()
debugFileCheckBackTracks infile = do
    contents <- readFile infile
    putStrLn . saltyToDebugTreeCheckBackTracks $ contents

findErrorInFile infile = do
  contents <- readFile infile
  putStrLn "error in:"
  putStrLn . findErrorLine $ contents

findErrorInStdin = do
  contents <- getContents
  putStrLn "error in:"
  putStrLn . findErrorLine $ contents

convertToPhpStdin = do
    contents <- getContents
    putStrLn $ saltyToPhp (getIndentAmt contents) contents

convertToJsStdin = do
    contents <- getContents
    putStrLn $ saltyToJs (getIndentAmt contents) contents

main = do
  args <- getArgs
  case args of
       ["-h"] -> printHelp
       ["help"] -> printHelp
       ["-d", inputFile] -> debugFile inputFile
       ["-b", inputFile] -> debugFileCheckBackTracks inputFile

       ["-e"] -> findErrorInStdin
       ["-e", inputFile] -> findErrorInFile inputFile

       ["-p"] -> convertToPhpStdin
       ["-p", inputFile] -> convertToPhpFile inputFile (replace ".salt" ".php" (replace ".saltphp" ".php" inputFile))

       ["-j"] -> convertToJsStdin
       ["-j", inputFile] -> convertToJsFile inputFile (replace ".salt" ".js" (replace ".saltjs" ".js" inputFile))

       [inputFile] -> convertPhpPrintToStdout inputFile
       _ -> printHelp
