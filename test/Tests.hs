import Test.HUnit

import ParserTests
import EvalTests

-- collects all tests together
spielTests :: Test
spielTests = TestList [parserTests,evalTests]

-- |
main :: IO Counts
main = runTestTT spielTests
