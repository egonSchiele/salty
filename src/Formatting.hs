module Formatting where

import Utils
import Types
import ToPhp
import Data.List (intercalate)

formatDebug :: [Salty] -> String
formatDebug list = unlines . indentDebug . (map strip) . lines . addNewlines . show $ list

addNewlines str = replace "[" "[\n" . replace "{" "{\n" . replace "," ",\n" . replace "}" "\n}" . replace "]" "\n]" $ str

addSemicolons :: [String] -> [String]
addSemicolons phpLines = for phpLines $ \line ->
                              if (line == "") || (last line) `elem` ['{', '}', ';']
                                 then line
                                 else (line ++ ";")

indent :: [String] -> [String]
indent lines_ = indent_ lines_ 0

indent_ :: [String] -> Int -> [String]
indent_ [] _ = []
indent_ ("":lines_) indentAmt = "":(indent_ lines_ indentAmt)
indent_ (l:lines_) indentAmt = newLine:(indent_ lines_ newAmt)
  where newLine = if (last l) == '}' || (last_ 2 l) == "};" || (head l) == '}'
                     then (replicate ((indentAmt-1)*4) ' ') ++ l
                     else (replicate (indentAmt*4) ' ') ++ l
        newAmt = newAmt_ l indentAmt


newAmt_ l indentAmt
  | (head l) == '}' && (last l) == '{' = indentAmt
  | (last l) == '{' = indentAmt + 1
  | (last l) == '}' = indentAmt - 1
  | (head l) == '}' = indentAmt - 1
  | (last_ 2 l) == "};" = indentAmt - 1
  | otherwise = indentAmt


indentDebug :: [String] -> [String]
indentDebug lines_ = indentDebug_ lines_ 0

indentDebug_ :: [String] -> Int -> [String]
indentDebug_ [] _ = []
indentDebug_ ("":lines_) indentAmt = "":(indentDebug_ lines_ indentAmt)
indentDebug_ (l:lines_) indentAmt = newLine:(indentDebug_ lines_ newAmt)
  where newLine = if (last l) `elem` ['}', ']'] || (last_ 2 l) `elem` ["},", "],", "})"] || (last_ 3 l) `elem` ["}),", "}))"]  || (last_ 4 l) `elem` ["})),"]
                     then (replicate ((indentAmt-1)*4) ' ') ++ l
                     else (replicate (indentAmt*4) ' ') ++ l
        newAmt = getNewAmt l indentAmt

getNewAmt l indentAmt
  | (last l) `elem` ['}', ']'] = indentAmt - 1
  | (last_ 2 l) `elem` ["},", "],", "})"] = indentAmt - 1
  | (last_ 3 l) `elem` ["}),", "}))"] = indentAmt - 1
  | (last_ 4 l) `elem` ["})),"] = indentAmt - 1
  | (last l) `elem` ['{', '['] = indentAmt + 1
  | otherwise = indentAmt

check_ :: Salty -> Salty
check_ (Parens l) = Parens (checkBackTracks l)
check_ x = x

checkBackTracks :: [Salty] -> [Salty]
checkBackTracks [] = []
checkBackTracks ((Operation (Parens l) o (Parens r)):xs) = (Operation (Parens (checkBackTracks l)) o (Parens (checkBackTracks r))):(checkBackTracks xs)
checkBackTracks ((Operation l o (Parens r)):xs) = (Operation l o (Parens (checkBackTracks r))):(checkBackTracks xs)
checkBackTracks ((Operation (Parens l) o r):xs) = (Operation (Parens (checkBackTracks l)) o r):(checkBackTracks xs)

checkBackTracks ((FunctionCall (Just (Parens o)) c a):xs) = (FunctionCall (Just (Parens (checkBackTracks o))) c a):(checkBackTracks xs)

checkBackTracks ((HigherOrderFunctionCall (Parens l) o (Parens r)):xs) =
    (HigherOrderFunctionCall (Parens (checkBackTracks l)) o (Parens (checkBackTracks r))):(checkBackTracks xs)

checkBackTracks ((HigherOrderFunctionCall l o (Parens r)):xs) =
    (HigherOrderFunctionCall l o (Parens (checkBackTracks r))):(checkBackTracks xs)
checkBackTracks ((HigherOrderFunctionCall (Parens l) o r):xs) =
    (HigherOrderFunctionCall (Parens (checkBackTracks l)) o r):(checkBackTracks xs)

checkBackTracks ((LambdaFunction args (Parens s)):xs) = (LambdaFunction args (Parens (checkBackTracks s))):(checkBackTracks xs)

checkBackTracks ((If c t Nothing):xs) = (If (check_ c) (check_ t) Nothing):(checkBackTracks xs)
checkBackTracks ((If c t (Just e)):xs) = (If (check_ c) (check_ t) (Just (check_ e))):(checkBackTracks xs)

checkBackTracks ((ReturnStatement s):xs) = (ReturnStatement (check_ s)):(checkBackTracks xs)
checkBackTracks ((Negate s):xs) = (Negate (check_ s)):(checkBackTracks xs)

checkBackTracks (x:(BackTrack s):xs) = s:(checkBackTracks xs)
checkBackTracks ((Function n a body):x) = (Function n a (checkBackTracks body)):(checkBackTracks x)
checkBackTracks ((Parens salties):x) = (Parens (checkBackTracks salties)):(checkBackTracks x)
checkBackTracks (x:[]) = [x]
checkBackTracks (x:xs) = x:(checkBackTracks xs)

saltyToPhp_ :: [Salty] -> String
saltyToPhp_ tree = unlines . indent . addSemicolons . lines . (intercalate "\n") . (map toPhp) . checkBackTracks . (filter (not . isSaltyComment)) $ tree

