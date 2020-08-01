module TypeCheckerTests where

--
-- TypeChecker Tests
--

import Test.HUnit
import Parser.Parser
import Language.Types
import qualified Data.Set as S
import Text.Parsec.Error
import Data.Either
import Text.Parsec
import Utils
import Typechecker.Typechecker
import Language.Syntax
import Typechecker.Monad

--
-- exported tests for the TypeChecker
--
typeCheckerTests :: Test
typeCheckerTests = TestList []

-- | Represents the rest result for typchecking examples
-- On Fail, return # failures & # errors for easier analysis in the cmd line
data TypeCheckerTestResult =
  Fail Int Int |
  Pass

-- | Allows displaying of a typechecker example checking result
instance Show TypeCheckerTestResult where
  show (Fail fails errs) = "Typechecking all Examples\n Failures: " ++ show fails ++ " Errors: " ++ show errs
  show Pass              = "TypeChecking all Examples\n Failures: 0 Errors: 0\n (Passed)"


-- | Determines whether a type checker result is passing or not
tcPassed :: TypeCheckerTestResult -> Bool
tcPassed (Fail _ _) = False
tcPassed Pass       = True

--
-- TEST STRUCTURE
--
-- testNameOfTest :: Test
-- testNameOfTest = TestCase (
--  assertEqual "Test Description"
--  ExpectedValue
--  ExpressionToCheck)
--

typeCheckAllExamples :: IO TypeCheckerTestResult
typeCheckAllExamples = do
   bglFiles <- getExampleFiles
   logTestStmt "Type checking:"
   mapM_ (putStrLn . ("\t" ++)) bglFiles
   results <- mapM parseGameFile bglFiles
   let parsed = rights results           -- the parser tests report these failures
       failures = filter (not . success) $ map tc parsed
       errs = map Typechecker.Typechecker.errors failures
   logTestStmt "Failures:"
   mapM_ (putStrLn . ("\n" ++)) (map showTCError (concat errs))
   let errCount = length errs
   let failCount= length failures
   return $ if errCount > 0 || failCount > 0 then (Fail failCount errCount) else Pass
