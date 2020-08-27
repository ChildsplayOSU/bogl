import Test.HUnit

import System.Exit (exitSuccess,exitFailure)
import ParserTests
import EvalTests
import TypeCheckerTests

-- collects all tests together
spielTests :: Test
spielTests = TestList [parserTests,evalTests,typeCheckerTests]

-- run all tests in the suite
main :: IO ()
main =  do
  result      <- runTestTT spielTests  -- Run standard HUnit tests
  parseResult <- checkParseAllExamples -- Custom test: parse all examples, return Pass | (Fail #failures)
  tcResult    <- typeCheckAllExamples  -- Custom test: tc all examples, return Pass | (Fail #failures #errors)
  _           <- typeCheckIll  -- type check ill typed examples (note: intentionally display only)
  (tttR, msg) <- evalTicTacToe
  let failed = or [(errors result) > 0, (failures result) > 0, not $ parsePassed parseResult, not $ tcPassed tcResult, not tttR]
  putStrLn ("\n\n" ++ show tcResult ++ "\n\n" ++ show parseResult ++ "\n") -- print example TC/Parse results for verification
  putStrLn $ "Evaluating tic tac toe led to: " ++ msg
  if failed then exitFailure else exitSuccess
