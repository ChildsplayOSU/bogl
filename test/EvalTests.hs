module EvalTests(evalTests, evalTicTacToe) where
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
import Language.Types
import Parser.Parser

evalTests :: Test
evalTests = TestList [
  testEvalEquiv,
  testEvalMod,
  testEvalNotEquiv,
  testIntSymbolNotEquiv,
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
  testNegativeBoardAccess,
  testBadPlace]

testEvalEquiv :: Test
testEvalEquiv = TestCase (
  assertEqual "Test equiv in eval"
  (Right (Vb False))
  (evalTest (eval (Binop Equiv (I 3) (I 4)))))

-- | Tests the mod can be correctly evaluated
testEvalMod :: Test
testEvalMod = TestCase (
  assertEqual "Test mod in eval"
  (Right (Vi 0))
  (evalTest (eval (Binop Mod (I 8) (I 2)))))

-- | Verifies that /= works for operands that are not equivalent
testEvalNotEquiv :: Test
testEvalNotEquiv = TestCase (
  assertEqual "Test not equiv in eval"
  (Right (Vb True))
  (evalTest (eval (Binop NotEquiv (I 92) (I 64)))))

testIntSymbolNotEquiv :: Test
testIntSymbolNotEquiv = TestCase (
  assertEqual "Test eval not equiv with Int and Symbol"
  (Right (Vb True))
  (evalTest (eval (Binop NotEquiv (I 92) (S "X")))))

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
  (isRightErr (let _valdef = (Vf ["x"] [] (If (Binop Less (Ref "x") (I 6000)) (App "iloop" (Tuple [(Binop Plus (Ref "x") (I 1))])) (Ref "x"))) in
     let env    = Env [("iloop", _valdef)] (1,1) in
     let buffer = ([],[],1) in
     let evalVal= eval (App "iloop" (Tuple [(I 0)])) in
     runEval env buffer evalVal)))

-- | Tests that negative board access doesn't crash out things
testNegativeBoardAccess :: Test
testNegativeBoardAccess = TestCase (
  assertEqual "Test that evaluating a negative board position gives an appropriate error"
  True
  (isRightErr (let barray = array ((1,1),(1,1)) [((1,1),(Vi 1))] in
   let _board  = Vboard barray in
   let env    = Env [("b", _board)] (1,1) in
   let buffer = ([],[],1) in
   let evalVal= eval (Ref "b!(1,-1)") in
   runEval env buffer evalVal)))

-- | Evaluate tic tac toe with a series of moves that leads to X winning
--   This is reflected as the output Vs "X"
evalTicTacToe :: IO (Bool, String)
evalTicTacToe = do
   res <- parseGameFile $ examplesPath ++ "TicTacToe.bgl"
   case res of
      (Left _) -> return (False, "TicTacToe parse error")
      (Right (Game _ (BoardDef (szx, szy) _) _ vs)) -> return val
         where
            val        = case evalResult of
                           (Right (_, r)) -> (r == Vs "X", show r)
                           e              -> (False, show e)
            evalResult = runWithBuffer (bindings_ (szx, szy) vs) buf (Ref "result")
            buf        = (moves, [], 0)
            moves      = map Vt (map (\_p -> [Vi (fst _p), Vi (snd _p)]) coords)
            coords     = [(1, 1), (2, 1), (2, 2), (3, 1), (3, 3)]


-- | Test that place function is not allowed to place outside the board
testBadPlace :: Test
testBadPlace = TestCase (
  assertEqual "Tests that the 'place' function won't crash when out of bounds"
  True
  (let barray = array ((1,1),(1,1)) [((1,1),(Vi 1))] in
   let _board  = Vboard barray in
   let evalTT = runEval (Env [("b",_board)] (1,1)) ([], [], 1) in
   isRightErr (evalTT (eval (App "place" (Tuple [(I 1),(Ref "b"),(Tuple [(I 1),(I 2)])]))))))
