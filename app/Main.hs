module Main where

import System.Environment
import System.Directory

import Parser
import ParserPhp (phpToPhp)
import Utils
import Print (print2)

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
    if out /= ""
       then putStrLn $ print2 "\"%\" : \"%\"," out infile
       else return ()

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

readFromStdin = do
    contents <- getContents
    putStrLn $ saltyToPhp (getIndentAmt contents) contents

convertToJs = do
    contents <- getContents
    putStrLn $ saltyToJs (getIndentAmt contents) contents

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
       ["-b", inputFile] -> debugFileCheckBackTracks inputFile
       ["debug"] -> debugFromStdin
       ["-d"] -> debugFromStdin
       ["-e"] -> findErrorInStdin
       ["-f", inputFile] -> convertToFile inputFile (replace ".salt" ".php" inputFile)
       ["-e", inputFile] -> findErrorInFile inputFile
       ["-p", inputFile] -> convertToPhp inputFile
       ["-j"] -> convertToJs
       ["-j", inputFile] -> convertToJsFile inputFile (replace ".salt" ".js" inputFile)
       [inputFile] -> convert inputFile
       [] -> readFromStdin
       _ -> printHelp
