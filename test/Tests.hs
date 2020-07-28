import Test.HUnit

import System.Exit (exitSuccess,exitFailure)
import ParserTests
import EvalTests
import TypeCheckerTests

-- collects all tests together
spielTests :: Test
spielTests = TestList [parserTests,evalTests]

--data Counts = Counts { cases, tried, errors, failures :: Int }
-- deriving (Eq, Show, Read)

-- run all tests in the suite
main :: IO ()
main =  do
  result <- runTestTT spielTests
  parseSuccess <- checkParseAllExamples
  tcSuccess    <- typeCheckAllExamples
  let failed = or [(errors result) > 0, (failures result) > 0, not parseSuccess, not tcSuccess]
  if failed then exitFailure else exitSuccess
