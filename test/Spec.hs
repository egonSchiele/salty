import Test.HUnit
import Lib

matches str1 str2 = TestCase $ assertEqual "" str2 (build str1)



tests = TestList [
    "@foo = 1" `matches` "$this->foo = 1",
    "@@foo = 1" `matches` "self::$foo = 1",
    "@@build a b := 2" `matches` "static function build($a, $b) {\n\treturn 2;\n}"
  ]


main :: IO ()
main = runTestTT tests >> return ()
