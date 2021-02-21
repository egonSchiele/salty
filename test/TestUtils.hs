{-# LANGUAGE QuasiQuotes #-}
module TestUtils where
import Test.HUnit
import qualified Parser

assertEqual_ expected actual = assertBool failureMsg (expected == actual)
  where failureMsg = "expected:\n" ++ expected ++"\nbut got actual:\n" ++ actual ++ "\n"

-- prints out the error on multiple lines
matchesJs str1 str2 = TestCase $ assertEqual_ str2 (Parser.saltyToJs 0 str1)
