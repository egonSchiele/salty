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

checkBackTracks :: [Salty] -> [Salty]
checkBackTracks [] = []
checkBackTracks (a:(BackTrack s):xs) = s:(checkBackTracks xs)
checkBackTracks (a:(WithNewLine (BackTrack s)):xs) = (WithNewLine s):(checkBackTracks xs)
checkBackTracks (x:xs) = (checkBackTracksSingle x):(checkBackTracks xs)

checkBackTracksSingle :: Salty -> Salty
checkBackTracksSingle (Operation l o r) = Operation (checkBackTracksSingle l) o (checkBackTracksSingle r)
checkBackTracksSingle (Function n a b) = Function n a (checkBackTracks b)
checkBackTracksSingle fc@(FunctionCall Nothing cn ca) = fc
checkBackTracksSingle fc@(FunctionCall (Just o) cn ca) = FunctionCall (Just (checkBackTracksSingle o)) cn ca
checkBackTracksSingle (HigherOrderFunctionCall o cn f) = HigherOrderFunctionCall (checkBackTracksSingle o) cn (checkBackTracksSingle f)
checkBackTracksSingle (LambdaFunction args b) = LambdaFunction args (checkBackTracksSingle b)
checkBackTracksSingle (If c t Nothing)  = If (checkBackTracksSingle c) (checkBackTracksSingle t) Nothing
checkBackTracksSingle (If c t (Just e))  = If (checkBackTracksSingle c) (checkBackTracksSingle t) (Just (checkBackTracksSingle e))
checkBackTracksSingle (ReturnStatement s) = ReturnStatement (checkBackTracksSingle s)
checkBackTracksSingle (Negate s) = Negate (checkBackTracksSingle s)
checkBackTracksSingle (WithNewLine s) = WithNewLine (checkBackTracksSingle s)
checkBackTracksSingle (Parens s) = Parens (checkBackTracks s)
checkBackTracksSingle (Braces s) = Braces (checkBackTracks s)
checkBackTracksSingle (BackTrack s) =  BackTrack (checkBackTracksSingle s)
checkBackTracksSingle (HashLookup h k) =  HashLookup (checkBackTracksSingle h) (checkBackTracksSingle k)
checkBackTracksSingle x = x

saltyToPhp_ :: [Salty] -> String
saltyToPhp_ tree = unlines . indent . addSemicolons . removeBlanks . lines . (intercalate "\n") . (map toPhp) . checkBackTracks . (filter (not . isSaltyComment)) $ tree

removeBlanks list = filter (\item -> (not (item `elem` ["", "\n", ";"]))) list
