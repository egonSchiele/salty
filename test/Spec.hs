{-# LANGUAGE QuasiQuotes #-}
import Test.HUnit
import qualified PhpSpec

allTests = TestList $
              PhpSpec.phpTests

main :: IO ()
main = runTestTT allTests >> return ()
