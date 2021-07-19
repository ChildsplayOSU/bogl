module TypeCheckerTests where

--
-- TypeChecker Tests
--

import Test.HUnit
import Parser.Parser
import Language.Types
import Data.Either
import Utils
import Typechecker.Typechecker
import Typechecker.Utils
import Error.TypeError
import Error.Error
import Language.Syntax
import Typechecker.Monad
import System.Directory
import System.FilePath

import Text.Parsec hiding (Error)
import Text.Parsec.Pos

--
-- exported tests for the TypeChecker
--
typeCheckerTests :: Test
typeCheckerTests = TestList
   [
     testSmallLiteralBoardEq
   , testLargeLiteralBoardEq
   , testOutOfBoundsLiteralGet
   , testBadBinopCompare
   , testBadBinop
   , testIncompleteBoardEq
   , testOneSpaceIncompleteBoardEq
   , testCompleteBoardEq
   , testBoardTypeMismatch
   , testFeqMismatch
   , testArgCountMismatch
   , testTuples
   ]

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

-- | A dummy position with which to annotate test cases
dummyPos :: SourcePos
dummyPos = initialPos ""

-- | Check that every Game in a list type checks
allPassTC :: [Game SourcePos] -> Bool
allPassTC = and . map success . map tc

-- | Check that every Game in a list fails to type check
allFailTC :: [Game SourcePos] -> Bool
allFailTC = and . map (not . success) . map tc

-- | Check that every Expr in a list fails to type check
allFailTCexpr :: Env -> [Expr SourcePos] -> Bool
allFailTCexpr = \_e -> and . map isLeft . map (tcexpr _e)

-- | Check that every Expr in a list successfully type checks
allPassTCexpr :: Env -> [Expr SourcePos] -> Bool
allPassTCexpr = \_e -> and . map isRight . map (tcexpr _e)

testOutOfBoundsLiteralGet :: Test
testOutOfBoundsLiteralGet = TestCase (
   assertBool "board access with integer literal that is out of bounds type checks" $
   allFailTCexpr env [nx, ny, zx, zy, gx, gy]
   )
   where
      env = exampleEnv { types = ("b", boardt) : types exampleEnv }
      get = \(x, y) -> Binop Get (Ref "b") (Tuple [I x, I y])
      nx = get (-1, 1)
      ny = get (1, -1)
      zx = get (0, 1)
      zy = get (1, 0)
      (mx, my) = Typechecker.Monad.size exampleEnv
      gx = get (mx + 1, 1)
      gy = get (mx, my + 1)

testSmallLiteralBoardEq :: Test
testSmallLiteralBoardEq = TestCase (
   assertBool "BoardEq with integer literal that is <= 0 type checks" $
   allFailTC (map (testGame . \x -> [x]) [z, n, n2])
   )
   where
     z = BVal (Sig "b1" (Plain boardxt)) [PosDef "b1" (Index 0) (Index 0) (I 1)] dummyPos
     n  = BVal (Sig "b1" (Plain boardxt)) [PosDef "bn" (Index (-1)) (ForAll "y") (I 1)] dummyPos
     n2 = BVal (Sig "b1" (Plain boardxt)) [PosDef "bn" (ForAll ("x")) (Index (-10)) (I 1)] dummyPos

testLargeLiteralBoardEq :: Test
testLargeLiteralBoardEq = TestCase (
   assertBool "BoardEq with integer literal that is greater than board size type checks" $
   allFailTC (map (testGame . \x -> [x]) [a, b])
   )
   where
     a = BVal (Sig "b1" (Plain boardxt)) [PosDef "b1" (Index 10) (Index 1) (I 1)] dummyPos
     b = BVal (Sig "b1" (Plain boardxt)) [PosDef "b1" (Index 1) (Index 10) (I 1)] dummyPos

testBadBinopCompare :: Test
testBadBinopCompare = TestCase (
   assertBool "equality across non-symbol types fails type check" $
   allFailTCexpr env [a, b, c]
   )
   where
      env = exampleEnv { types = ("b", boardt) : types exampleEnv }
      a = _equiv (I 1) (B True)
      b = _equiv (I 1) (Ref "b")
      c = _equiv (Ref "b") (B False)
      _equiv = \l r -> Binop Equiv l r

testBadBinop :: Test
testBadBinop = TestCase (
   assertBool "invalid binary expressions fail type check" $
   allFailTCexpr exampleEnv [a, b, c]
   )
   where
      _op = \l o r -> Binop o l r
      a  = _op (S "X") Geq (I 1)
      b  = _op (S "X") Plus (I 1)
      c  = _op (B True) Plus (B False)

-- | Checks that expressions have a given type in a given program
tcExprsInGames :: [(FilePath, ExprS, Xtype)] -> IO [(String, Either Error Xtype, Xtype)]
tcExprsInGames i = do
   res <- mapM check i
   return $ filter (\(_,b,c) -> wrongType b c) res
   where
      wrongType (Left _) _    = True
      wrongType (Right xa) xe = xa /= xe
      check :: (FilePath, ExprS, Xtype) -> IO (String, Either Error Xtype, Xtype)
      check (fp, es, et) = do
         at <- tcInGame fp es
         return (fp ++ ":" ++ es, at, et)

-- | A list of filepaths, expressions to be type checked in those programs, and their expected types
exprTypes :: [(FilePath, ExprS, Xtype)]
exprTypes = map (\(a, b, c) -> (examplesPath ++ a, b, c)) exprs
   where
      exprs = [("TicTacToe.bgl", "1 + 1", intxt)
              ]

typeCheckAllExamples :: IO TypeCheckerTestResult
typeCheckAllExamples = do
   bglFiles <- getExampleFiles
   logTestStmt "Type checking:"
   mapM_ (putStrLn . ("\t" ++)) bglFiles
   results <- mapM parseGameFile bglFiles
   let parsed = rights results           -- the parser tests report these failures
       _failures = filter (not . success) $ map tc parsed
       errs = map Typechecker.Typechecker.errors _failures
   failedExprs <- tcExprsInGames exprTypes
   logTestStmt "Failures:"
   mapM_ (putStrLn . ("\n" ++)) (map showTCError (concat errs))
   logTestStmt "Expresions that do not have the expected type:"
   putStrLn (show failedExprs)
   let errCount      = length errs
       failCount     = length _failures
       exprFailCount = length failedExprs
   return $ if any (> 0) [errCount, failCount, exprFailCount] then (Fail failCount errCount) else Pass

illTypedPath :: String
illTypedPath = examplesPath ++ "illTyped/"

illTypedFiles :: IO [String]
illTypedFiles = do
   files  <- listDirectory illTypedPath
   let fullPaths = (map ((++) illTypedPath) files)
       bglFiles  = filter (isExtensionOf ".bgl") (fullPaths)
   return bglFiles

typeCheckIll :: IO Int
typeCheckIll = do
   bglFiles <- illTypedFiles
   logTestStmt "Type checking (ill typed):"
   mapM_ (putStrLn . ("\t" ++)) bglFiles
   results <- mapM parseGameFile bglFiles
   let parsed = zip bglFiles $ rights results -- assume they all parsed correctly
       succs  = filter (success . snd) $ map (\_p -> (fst _p, (tc . snd) _p)) parsed
   logTestStmt "Incorrect successes:"
   mapM_ (putStrLn . ("\t" ++)) $ map (\_p -> fst _p ++ "\n\t\t" ++ (show (((rtypes . snd) _p)))) succs
   return $ length succs

-- test TC on incomplete board
testIncompleteBoardEq :: Test
testIncompleteBoardEq = TestCase (
  (assertBool "Verifies that an incomplete board equation is disallowed") $
  allFailTC [testGame [(BVal (Sig "b" (Plain boardxt)) [PosDef "b" (ForAll ("x")) (Index 1) (I 1)] dummyPos)]]
  )

-- test all but one place is still an invalid board
testOneSpaceIncompleteBoardEq :: Test
testOneSpaceIncompleteBoardEq = TestCase (
  (assertBool "Verifies that an incomplete board equation is disallowed") $
  allFailTC [testGame [(BVal (Sig "b" (Plain boardxt)) [
    PosDef "b" (ForAll ("x")) (Index 1) (I 1),
    PosDef "b" (ForAll ("x")) (Index 2) (I 1),
    PosDef "b" (ForAll ("x")) (Index 3) (I 1),
    PosDef "b" (ForAll ("x")) (Index 4) (I 1),
    PosDef "b" (Index (1)) (Index 5) (I 1),
    PosDef "b" (Index (2)) (Index 5) (I 1),
    PosDef "b" (Index (3)) (Index 5) (I 1),
    PosDef "b" (Index (4)) (Index 5) (I 1)
    -- missing (5,5) should constitute an incomplete board
  ] dummyPos)]])

-- test TC on complete board
testCompleteBoardEq :: Test
testCompleteBoardEq = TestCase (
  (assertBool "Verifies that a complete board equation is valid") $
  allPassTC [testGame [(BVal (Sig "b" (Plain boardxt)) [PosDef "b" (ForAll ("x")) (ForAll ("y")) (I 1)] dummyPos)]]
  )

testFeqMismatch :: Test
testFeqMismatch = TestCase (
  (assertBool "Verifies that a function equation is not allowed for a value equation signature") $
  let eqn = (Val (Sig "b" (Plain boardxt)) (Feq "b" (Pars ["x", "y"]) (I 1)) dummyPos) in
  case tc $ testGame [eqn] of
    (Tc False _ [(_, Error (TE (SigBadFeq _ _ _)) _ _)] _) -> True
    _                                      -> False
  )

-- | Test TC on expression for board equation where there should be a type mismatch
testBoardTypeMismatch :: Test
testBoardTypeMismatch = TestCase (
  assertBool "Verifies that a board type with an incorrect expr type reports a mismatch properly" $
  let beqn = (BVal (Sig "b" (Plain boardxt)) [PosDef "b" (ForAll ("x")) (ForAll ("y")) (B True)] dummyPos) in
  case tc $ testGame [beqn] of
    -- failed w/ Mismatch as expected (good)
    (Tc False _ [(_, Error (TE (Mismatch _ _ _)) _ _)] _) -> True
    -- anything else, pass or fail, is incorrect
    _                                      -> False)

-- | Test TC on equation with # of args that doesn't match the arg # in the type
-- f : Int -> Int
-- f(x,y) = 1
testArgCountMismatch :: Test
testArgCountMismatch = TestCase (
  assertBool "Verify that if arg count & input type count do not match, a type error is reported" $
  let aeqn = (Val (Sig "f" (Function (Ft intxt intxt))) (Feq "f" (Pars ["x","y"]) (I 1)) dummyPos) in
  case tc $ testGame [aeqn] of
    (Tc False _ [(_, Error (TE (Unknown _)) _ _)] _) -> True
    _                                                -> False
  )

-- | Tuple typechecking tests
testTuples :: Test
testTuples = TestLabel "Tuple TypeChecking Tests" (TestList [
  testTupleTCPasses,
  testTupleTCFailures
  ])

-- | Various tuple expressions that should be type correct
testTupleTCPasses :: Test
testTupleTCPasses = TestCase (
  assertBool "Test that type-correct exprs for tuples pass tc" $
  allPassTCexpr exampleEnv [a,b,c]
  )
  where
    a = Binop Equiv (Tuple [I 1, I 2]) (Tuple [I 2, I 3])
    b = Binop Equiv (Tuple [I 1, I 2]) (Tuple [I 1, I 2])
    c = Binop Equiv (Tuple [B False, I 2]) (Tuple [B True, I 24])

-- | Various tuple expressions that should not be type correct
testTupleTCFailures :: Test
testTupleTCFailures = TestCase (
  assertBool "Test that type-incorrect exprs for tuples fail tc" $
  allFailTCexpr exampleEnv [a,b,c]
  )
  where
    a = Binop Equiv (Tuple [I 1, I 2]) (Tuple [I 2, I 3, I 5])
    b = Binop Equiv (Tuple [I 1, I 2, I 5]) (Tuple [I 1, I 2])
    c = Binop Equiv (Tuple [B False, I 2]) (Tuple [I 24, B True])
