module FormattingJs where

import Utils
import Types
import ToPhp
import ToJs
import Data.List
import Data.Maybe
import Data.Char (isSpace)

rstrip = reverse . dropWhile isSpace . reverse

findString :: (Eq a) => [a] -> [a] -> Int
findString search str = fromMaybe (-1) $ findIndex (isPrefixOf search) (tails str)

formatDebug :: [Salty] -> String
formatDebug list = unlines . indentDebug . (map strip) . lines . addNewlines . show $ list

formatDebugStripBackTracks :: [Salty] -> String
formatDebugStripBackTracks list = unlines . indentDebug . (map strip) . lines . addNewlines . show . checkBackTracks $ list

addNewlines str = replace "[" "[\n" . replace "{" "{\n" . replace "," ",\n" . replace "}" "\n}" . replace "]" "\n]" $ str

addSemicolons :: [String] -> [String]
addSemicolons phpLines = for phpLines $ \line ->
                              if (line == "") || (last line) `elem` ['{', '}', ';', ',', '[', ':'] || (head line) `elem` ['/', '*'] || (first_ 2 line) `elem` [" *"] || ( ((findString "<=>" line) == -1) && ((findString "=>" line) > -1) ) || ((findString ":" line) > -1)
                                 then line
                                 else (line ++ ";")

indent :: Int -> [String] -> [String]
indent startAmt lines_ = indent_ lines_ startAmt

indent_ :: [String] -> Int -> [String]
indent_ [] _ = []
indent_ ("":lines_) indentAmt = "":(indent_ lines_ indentAmt)
indent_ (l:lines_) indentAmt = newLine:(indent_ lines_ newAmt)
  where newLine = if (last l) == '}' || l == "};" || (head l) == '}'|| l == "];"
                     then (replicate ((indentAmt-1)*2) ' ') ++ l
                     else (replicate (indentAmt*2) ' ') ++ l
        newAmt = newAmt_ l indentAmt


newAmt_ l indentAmt
  | (head l) == '}' && (last l) == '{' = indentAmt
  | (last l) == '{' || (last l) == '[' = indentAmt + 1
  | (head l) == '[' && length l == 1 = indentAmt + 1
  | (head l) == '{' && length l == 1 = indentAmt + 1
  | (last l) == '}' || (last l) == ']' = indentAmt - 1
  | (head l) == '}' || (head l) == ']' = indentAmt - 1
  | l == "};" || l == "];" = indentAmt - 1
  | otherwise = indentAmt


indentDebug :: [String] -> [String]
indentDebug lines_ = indentDebug_ lines_ 0

indentDebug_ :: [String] -> Int -> [String]
indentDebug_ [] _ = []
indentDebug_ ("":lines_) indentAmt = "":(indentDebug_ lines_ indentAmt)
indentDebug_ (l:lines_) indentAmt = newLine:(indentDebug_ lines_ newAmt)
  where newLine = if (last l) `elem` ['}', ']'] || (last_ 2 l) `elem` ["},", "],", "})"] || (last_ 3 l) `elem` ["}),", "}))"]  || (last_ 4 l) `elem` ["})),"]
                     then (replicate ((indentAmt-1)*2) ' ') ++ l
                     else (replicate (indentAmt*2) ' ') ++ l
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
checkBackTracks ((Operation left op right):(BackTrack s):xs) = checkBackTracks ((Operation left op s):xs)
checkBackTracks ((MultiAssign vars right):(BackTrack s):xs) = checkBackTracks ((MultiAssign vars s):xs)
checkBackTracks (a:(BackTrack s):xs) = checkBackTracks (s:xs)
checkBackTracks ((Operation left op right):(WithNewLine (BackTrack s)):xs) = checkBackTracks ((WithNewLine (Operation left op s)):xs)
checkBackTracks ((MultiAssign vars right):(WithNewLine (BackTrack s)):xs) = checkBackTracks ((WithNewLine (MultiAssign vars s)):xs)
checkBackTracks (a:(WithNewLine (BackTrack s)):xs) = checkBackTracks ((WithNewLine s):xs)

-- try using
-- @shops().find(1).each(\\s -> s.user)
-- without the line below...
checkBackTracks (a:(WithNewLine hof@(HigherOrderFunctionCall (BackTrack _) _ _ _)):xs) = checkBackTracks ((WithNewLine hof):xs)
checkBackTracks (x:xs) = (checkBackTracksSingle x):(checkBackTracks xs)

checkBackTracksSingle :: Salty -> Salty
checkBackTracksSingle (Operation l o r) = Operation (checkBackTracksSingle l) o (checkBackTracksSingle r)
checkBackTracksSingle (Function n a b v s) = Function n a (checkBackTracks b) v s
checkBackTracksSingle (Guard cond outcome) = Guard (checkBackTracks cond) (checkBackTracks outcome)
checkBackTracksSingle (SaltyGuard val guards) = SaltyGuard val (checkBackTracks guards)
checkBackTracksSingle (FunctionCall Nothing cn ca) = FunctionCall Nothing cn (checkBackTracks ca)
checkBackTracksSingle (FunctionCall (Just o) cn ca) = FunctionCall (Just (checkBackTracksSingle o)) cn (checkBackTracks ca)
checkBackTracksSingle (HigherOrderFunctionCall o cn f a) = HigherOrderFunctionCall (checkBackTracksSingle o) cn (checkBackTracksSingle f) a
checkBackTracksSingle (LambdaFunction args b) = LambdaFunction args (checkBackTracksSingle b)
checkBackTracksSingle (If c t Nothing)  = If (checkBackTracks c) (checkBackTracksSingle t) Nothing
checkBackTracksSingle (If c t (Just e))  = If (checkBackTracks c) (checkBackTracksSingle t) (Just (checkBackTracksSingle e))
checkBackTracksSingle (While c b) = While (checkBackTracksSingle c) (checkBackTracksSingle b)
checkBackTracksSingle (New c args) = New c (checkBackTracks args)
checkBackTracksSingle (Class n e i s) = Class n e i (checkBackTracksSingle s)
checkBackTracksSingle (ReturnStatement s) = ReturnStatement (checkBackTracksSingle s)
checkBackTracksSingle (Negate s) = Negate (checkBackTracksSingle s)
checkBackTracksSingle (WithNewLine s) = WithNewLine (checkBackTracksSingle s)
checkBackTracksSingle (Parens s) = Parens (checkBackTracks s)
checkBackTracksSingle (Braces s) = Braces (checkBackTracks s)
checkBackTracksSingle (Array s) = Array (checkBackTracks s)
checkBackTracksSingle (BackTrack s) = (checkBackTracksSingle s)
checkBackTracksSingle (HashLookup h k) = HashLookup (checkBackTracksSingle h) (checkBackTracksSingle k)
checkBackTracksSingle x = x

saltyToJs_ :: Int -> [Salty] -> String
saltyToJs_ indentAmt tree = rstrip . unlines . (indent indentAmt) . addSemicolons . (map addBlanks) . removeBlanks . lines . concat . (map toJs) . checkBackTracks . (filter (not . isSaltyComment)) $ tree

removeBlanks list = filter (\item -> (not (item `elem` ["", "\n", ";"]))) list
addBlanks line
  | line == "<EMPTYLINE>" = ""
  | otherwise = line
