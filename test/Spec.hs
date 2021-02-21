{-# LANGUAGE QuasiQuotes #-}
import Test.HUnit
import qualified PhpSpec
import qualified JsSpec
import qualified LongExamples.Js.SimpleWebPage.Controller
import qualified LongExamples.Js.SimpleWebPage.Nav

allTests = TestList $
                 PhpSpec.phpTests
              ++ JsSpec.jsTests
              ++ LongExamples.Js.SimpleWebPage.Controller.tests
              ++ LongExamples.Js.SimpleWebPage.Nav.tests

main :: IO ()
main = runTestTT allTests >> return ()
