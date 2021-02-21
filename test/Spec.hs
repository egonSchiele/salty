{-# LANGUAGE QuasiQuotes #-}
import Test.HUnit
import qualified PhpSpec
import qualified JsSpec
import qualified LongExamples.Js.SimpleWebPage.Pages

allTests = TestList $
                 PhpSpec.phpTests
              ++ JsSpec.jsTests
              ++ LongExamples.Js.SimpleWebPage.Pages.tests

main :: IO ()
main = runTestTT allTests >> return ()
