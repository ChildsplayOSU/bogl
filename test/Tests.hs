import Test.HUnit

import System.Exit (exitSuccess,exitFailure)
import ParserTests
import EvalTests
import TypeCheckerTests
import APITests

-- collects all tests together
spielTests :: Test
spielTests = TestList [parserTests,evalTests,typeCheckerTests, apiTests]

-- run all tests in the suite
main :: IO ()
main =  do
  result      <- runTestTT spielTests  -- Run standard HUnit tests
  parseResult <- checkParseAllExamples -- Custom test: parse all examples, return Pass | (Fail #failures)
  tcResult    <- typeCheckAllExamples  -- Custom test: tc all examples, return Pass | (Fail #failures #errors)
  falsePos    <- typeCheckIll
  (tttR, msg) <- evalTicTacToe
  whileR <- evalWhile
  let badWhiles = filter (not . \(b, _, _) -> b) whileR
  let failed = or [(errors result) > 0, (failures result) > 0, not $ parsePassed parseResult, not $ tcPassed tcResult, falsePos > 0, not tttR, (not . null) badWhiles]
  putStrLn ("\n\n" ++ show tcResult ++ "\n\n" ++ show parseResult ++ "\n") -- print example TC/Parse results for verification
  putStrLn $ "Evaluating tic tac toe led to: " ++ msg
  if (not . null) badWhiles then putStrLn ("Bad while cases: \n" ++ (show badWhiles)) else return ()
  if failed then exitFailure else exitSuccess
