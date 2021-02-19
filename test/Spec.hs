{-# LANGUAGE QuasiQuotes #-}
import Test.HUnit
import qualified PhpSpec
import qualified JsSpec

allTests = TestList $
                 PhpSpec.phpTests
              -- ++ JsSpec.jsTests

main :: IO ()
main = runTestTT allTests >> return ()
