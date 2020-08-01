module EvalTests(evalTests) where
--
-- EvalTests.hs
--
-- Holds tests for eval and related functionality
--

import Test.HUnit
import Utils
import Runtime.Eval
import Language.Syntax
import Runtime.Monad
import Runtime.Values
import Data.Array

evalTests :: Test
evalTests = TestList [
  testEvalEquiv,
  testEvalNotEquiv,
  testWrongArgsInNotEquiv,
  testEvalNotEquivSymbols,
  testEvalEquivSymbols,
  testEvalLeqNotForSymbols,
  testEvalPlusMinusTimes,
  testBadTypesPlus,
  testEval2,
  testEvalTuple,
  testEvalLetRef,
  testEvalNextNotPresent,
  testEvalLimit,
  testNegativeBoardAccess]

testEvalEquiv :: Test
testEvalEquiv = TestCase (
  assertEqual "Test equiv in eval"
  (Right (Vb False))
  (evalTest (eval (Binop Equiv (I 3) (I 4)))))

-- | Verifies that /= works for operands that are not equivalent
testEvalNotEquiv :: Test
testEvalNotEquiv = TestCase (
  assertEqual "Test not equiv in eval"
  (Right (Vb True))
  (evalTest (eval (Binop NotEquiv (I 92) (I 64)))))

testWrongArgsInNotEquiv :: Test
testWrongArgsInNotEquiv = TestCase (
  assertEqual "Test bad args in not equiv via eval"
  True
  (isRightErr (evalTest (eval (Binop NotEquiv (I 92) (S "Oops"))))))

testEvalNotEquivSymbols :: Test
testEvalNotEquivSymbols = TestCase (
  assertEqual "Tests that /= works for symbols"
  (Right (Vb True))
  (evalTest (eval (Binop NotEquiv (S "A") (S "B")))))

testEvalEquivSymbols :: Test
testEvalEquivSymbols = TestCase (
  assertEqual "Tests that == works for symbols"
  (Right (Vb False))
  (evalTest (eval (Binop Equiv (S "A") (S "B")))))

testEvalLeqNotForSymbols :: Test
testEvalLeqNotForSymbols = TestCase (
  assertEqual "Tests that <= does not work for symbols"
  True
  (isRightErr (evalTest (eval (Binop Leq (S "A") (S "B"))))))

testEvalPlusMinusTimes :: Test
testEvalPlusMinusTimes = TestCase (
  assertEqual "Test plus, minus, times in eval"
  (Right (Vi 6))
  (evalTest (eval (Binop Plus (Binop Minus (I 1) (I 1)) (Binop Times (I 2) (I 3))))))


-- returns true on ERR value present on the right
-- helper used in testBadTypesPlus below
_unpackBadEither :: Either Exception Val -> Bool
_unpackBadEither (Right (Err _)) = True
_unpackBadEither (Left _) = False
_unpackBadEither (Right _) = False

testBadTypesPlus :: Test
testBadTypesPlus = TestCase (
  assertBool "Test bad add for bool and int"
  --(Right (Err "Could not do numerical operation on True to 2 * 3"))
  (_unpackBadEither (evalTest (eval (Binop Plus (B True) (Binop Times (I 2) (I 3)))))))


testEval2 :: Test
testEval2 = TestCase (
  assertEqual "Test eval of 2"
  (Right (Vi 2))
  (evalTest (eval (I 2))))

testEvalTuple :: Test
testEvalTuple = TestCase (
  assertEqual "Test eval tuple"
  (Right (Vt [(Vi 2),(Vi 3),(Vi 4)]))
  (evalTest (eval (Tuple [I 2, I 3, I 4]))))

testEvalLetRef :: Test
testEvalLetRef = TestCase (
  assertEqual "Test eval let and ref"
  (Right (Vi 2))
  (evalTest (eval (Let "x" (I 2) (Ref "x")))))

testEvalNextNotPresent :: Test
testEvalNextNotPresent = TestCase (
  assertEqual "Test 'next' not builtin anymore"
  True
  (isRightErr (evalTest (eval (App "next" (Tuple [(S "X")]))))))

-- | Tests that the evaluation limit is enforced after 5k iterations
-- This will naturally stop at 6k iterations, in case the loop is not terminated forcibly
testEvalLimit :: Test
testEvalLimit = TestCase (
  assertEqual "Test that the evaluation limit works"
  True
  (isRightErr (let valdef = (Vf ["x"] [] (If (Binop Less (Ref "x") (I 6000)) (App "iloop" (Tuple [(Binop Plus (Ref "x") (I 1))])) (Ref "x"))) in
     let env    = Env [("iloop", valdef)] (1,1) in
     let buffer = ([],[],1) in
     let evalVal= eval (App "iloop" (Tuple [(I 0)])) in
     runEval env buffer evalVal)))

-- | Tests that negative board access doesn't crash out things
testNegativeBoardAccess :: Test
testNegativeBoardAccess = TestCase (
  assertEqual "Test that evaluating a negative board position gives an appropriate error"
  True
  (isRightErr (let barray = array ((1,1),(1,1)) [((1,1),(Vi 1))] in
   let board  = Vboard barray in
   let env    = Env [("b", board)] (1,1) in
   let buffer = ([],[],1) in
   let evalVal= eval (Ref "b!(1,-1)") in
   runEval env buffer evalVal)))
