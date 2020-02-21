import Test.HUnit

import ParserTests
import EvalTests

-- collects all tests together
spielTests :: Test
spielTests = TestList [parserTests,evalTests]

-- run all tests in the suite
main :: IO Counts
main = runTestTT spielTests
