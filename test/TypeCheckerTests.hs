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

--
-- TEST STRUCTURE
--
-- testNameOfTest :: Test
-- testNameOfTest = TestCase (
--  assertEqual "Test Description"
--  ExpectedValue
--  ExpressionToCheck)
--

typeCheckAllExamples :: IO Bool
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
   return $ null failures
