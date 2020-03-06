import Test.HUnit

import System.Exit (exitSuccess,exitFailure)
import ParserTests
import EvalTests

-- collects all tests together
spielTests :: Test
spielTests = TestList [parserTests,evalTests]

--data Counts = Counts { cases, tried, errors, failures :: Int }
-- deriving (Eq, Show, Read)

-- run all tests in the suite
main :: IO ()
main =  do
  result <- runTestTT spielTests
  if (errors result) > 0 || (failures result) > 0 then exitFailure else exitSuccess
