module EvalTests (evalTests, evalTicTacToe, evalWhile, evalScope) where
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
import qualified Data.Map.Strict as Map()
import Error.RuntimeError

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
  testBadPlace,
  testEvalEqDoesntHideError,
  testEvalNumOpDoesntHideError,
  testEvalCompareOpDoesntHideError,
  testEvalTupleProjections]

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
  assertBool "Tests that <= does not work for symbols"
  (matchesRuntimeError (evalTest (eval (Binop Leq (S "A") (S "B")))) (BadComparison "A" "B")))

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
  (matchesRuntimeError (evalTest (eval (Binop Plus (B True) (Binop Times (I 2) (I 3))))) (BadNumericalOp "True" "2 * 3")))


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
  assertBool "Test 'next' not builtin anymore"
  (matchesRuntimeError (evalTest (eval (App "next" (Tuple [(S "X")])))) (UndefinedReference "next")))

-- | Tests that the evaluation limit is enforced after 5k iterations
-- This will naturally stop at 6k iterations, in case the loop is not terminated forcibly
testEvalLimit :: Test
testEvalLimit = TestCase (
  assertBool "Test that the evaluation limit works"
  (matchesRuntimeError (let _valdef = (Vf ["x"] emptyEvalEnv (If (Binop Less (Ref "x") (I 6000)) (App "iloop" (Tuple [(Binop Plus (Ref "x") (I 1))])) (Ref "x"))) in
     let env    = Env (evalEnvFromList [("iloop", _valdef)]) (1,1) in
     let buffer = ([],[],1) in
     let evalVal= eval (App "iloop" (Tuple [(I 0)])) in
     runEval env buffer evalVal) StackOverflow))

-- | Tests that negative board access doesn't crash out things
testNegativeBoardAccess :: Test
testNegativeBoardAccess = TestCase (
  assertBool "Test that evaluating a negative board position gives an appropriate error" $
  matchesRuntimeError (let barray = array ((1,1),(1,1)) [((1,1),(Vi 1))] in
   let _board  = Vboard barray in
   let env    = Env (evalEnvFromList [("b", _board)]) (1,1) in
   let buffer = ([],[],1) in
   let evalVal= eval (Binop Get (Ref "b") (Tuple [I 1, I $ -1])) in
   runEval env buffer evalVal) (InvalidBoardAccess (1,-1) (1,1)))

-- | Evaluate tic tac toe with a series of moves that leads to X winning
--   This is reflected as the output Vs "X"
evalTicTacToe :: IO (Bool, String)
evalTicTacToe = do
   [(b, _, r)] <- evalFile (examplesPath ++ "TicTacToe.bgl") [("result", Vs "X")] buf
   return (b, r)
   where
      buf        = (moves, [], 0)
      moves      = map Vt (map (\_p -> [Vi (fst _p), Vi (snd _p)]) coords)
      coords     = [(1, 1), (2, 1), (2, 2), (3, 1), (3, 3)]

-- | Evaluates many different expressions that contain while loops
evalWhile :: IO [(Bool, String, String)]
evalWhile = evalFile (examplesPath ++ "While.bgl") vs ([], [], 0)
   where
      vs = [("false", Vb False), ("ten1", Vi 10), ("ten2", Vi 10), ("ten3", Vi 10),
            ("tenOne", Vt [Vi 10, Vi 1]), ("ten4", Vi 10), ("thirty", Vi 30), ("twenty", Vi 20),
            ("eleven", Vi 11), ("five", Vi 5), ("fifteen", Vi 15), ("twentyNine", Vi 29)]

-- | Evaluates many different expressions that contain while loops
evalScope :: IO [(Bool, String, String)]
evalScope = evalFile (examplesPath ++ "Scope.bgl") vs ([], [], 0)
   where
      vs = [("eleven", Vi 11), ("eleven'", Vi 11)]

-- | Takes a file name, buffer, [(veq names, expected values)]
--   parses the file, evaluates the veqs and returns the result and some information
evalFile :: String -> [(String, Val)] -> Buffer -> IO [(Bool, String, String)]
evalFile fn l buf = do
   res <- parseGameFile fn
   case res of
      (Left _) -> return [(False, fn ++ " parse error", "")]
      (Right (Game _ (BoardDef (szx, szy) _) _ vs _)) -> return $ map check l
         where
            check (eqName, expected) = case run (Ref eqName) of
                     (Right (_, actual)) -> (expected == actual, fn ++ ":" ++ eqName, show actual)
                     e              -> (False, fn ++ ":" ++ eqName, show e)
            run = runWithBuffer (bindings_ (szx, szy) vs) buf

-- | Test that place function is not allowed to place outside the board
testBadPlace :: Test
testBadPlace = TestCase (
  assertBool "Tests that the 'place' function won't crash when out of bounds"
  (let barray = array ((1,1),(1,1)) [((1,1),(Vi 1))] in
   let _board  = Vboard barray in
   let evalTT = runEval (Env (evalEnvFromList [("b",_board)]) (1,1)) ([], [], 1) in
   isRightErr (evalTT (eval (App "place" (Tuple [(I 1),(Ref "b"),(Tuple [(I 1),(I 2)])]))))))


-- | Tests that verifying a binop in if-then-else maintains errors
-- let x = -1 in if b!(m,1)==1 then 1 else 0
testEvalEqDoesntHideError :: Test
testEvalEqDoesntHideError = TestCase (
  assertBool "Tests that verifying a binop in if-then-else maintains errors"
  (matchesRuntimeError (let barray = array ((1,1),(1,1)) [((1,1),(Vi 1))] in
     let _board  = Vboard barray in
     let env    = Env (evalEnvFromList [("b", _board)]) (1,1) in
     let buffer = ([],[],1) in
     let evalVal= eval (Let "x" (I $ -1) (If (Binop Equiv (Binop Get (Ref "b") (Tuple [Ref "x", I 1])) (I 1)) (I 1) (I 0))) in
     runEval env buffer evalVal) (InvalidBoardAccess (-1,1) (1,1))))

-- | Tests that evaluating a num op doesn't hide errors
-- let x = -1 in if (b!(m,1) + 1)==1 then 1 else 0
testEvalNumOpDoesntHideError :: Test
testEvalNumOpDoesntHideError = TestCase (
  assertBool "Tests that evaluating a num op doesn't hide errors"
  (matchesRuntimeError (let barray = array ((1,1),(1,1)) [((1,1),(Vi 1))] in
     let _board  = Vboard barray in
     let env    = Env (evalEnvFromList [("b", _board)]) (1,1) in
     let buffer = ([],[],1) in
     let evalVal= eval (Let "x" (I $ -1) (If (Binop Equiv (Binop Plus (Binop Get (Ref "b") (Tuple [Ref "x", I 1])) (I 1)) (I 1)) (I 1) (I 0))) in
     runEval env buffer evalVal) (InvalidBoardAccess (-1,1) (1,1))))

-- | Tests that the comparing op doesn't hide errors
-- let x = -1 in if b!(m,1)>=1 then 1 else 0
testEvalCompareOpDoesntHideError :: Test
testEvalCompareOpDoesntHideError = TestCase (
  assertBool "Tests that the comparing op doesn't hide errors"
  (matchesRuntimeError (let barray = array ((1,1),(1,1)) [((1,1),(Vi 1))] in
     let _board  = Vboard barray in
     let env    = Env (evalEnvFromList [("b", _board)]) (1,1) in
     let buffer = ([],[],1) in
     let evalVal= eval (Let "x" (I $ -1) (If (Binop Geq (Binop Get (Ref "b") (Tuple [Ref "x", I 1])) (I 1)) (I 1) (I 0))) in
     runEval env buffer evalVal) (InvalidBoardAccess (-1,1) (1,1))))

--
-- | Tuple projection tests
--
testEvalTupleProjections :: Test
testEvalTupleProjections = TestLabel "Tuple Projection Tests" (TestList [
  testEvalSingleTupProj1,
  testEvalSingleTupProj2,
  testEvalSingleTupProjOutOfBounds1,
  testEvalSingleTupProjOutOfBounds2,
  testEvalTupProj1,
  testEvalTupProj2,
  testEvalTupProjOutOfBounds1,
  testEvalTupProjOutOfBounds2
  ])

-- | Evaluate a single tuple projection using a literal
testEvalSingleTupProj1 :: Test
testEvalSingleTupProj1 = TestCase (
  assertEqual "Tests that single tuple projections work for literals"
  (Right (Vi 2))
  (evalTest (eval $ (Binop Proj (Tuple [I 1, I 2, I 3]) (I 1)))))

-- | Evaluate a single tuple projection using a reference
testEvalSingleTupProj2 :: Test
testEvalSingleTupProj2 = TestCase (
  assertEqual "Tests that single tuple projections work for refs"
  (Right (Vi 3))
  (evalTest (eval $ (Let "x" (I 2) $ Binop Proj (Tuple [I 1, I 2, I 3]) (Ref "x")))))

-- | Evaluate a single tuple projection that is out of bounds negative
testEvalSingleTupProjOutOfBounds1 :: Test
testEvalSingleTupProjOutOfBounds1 = TestCase (
  assertBool "Tests that single tuple projection fails when below zero"
  (matchesRuntimeError
    (evalTest (eval $ (Binop Proj (Tuple [I 1, I 2, I 3]) (I $ -1))))
    (InvalidTupleProj (-1) 3)))

-- | Evaluate a single tuple projection that is out of bounds positive
testEvalSingleTupProjOutOfBounds2 :: Test
testEvalSingleTupProjOutOfBounds2 = TestCase (
  assertBool "Tests that single tuple projection fails when out of bounds"
  (matchesRuntimeError
    (evalTest (eval $ (Binop Proj (Tuple [I 1, I 2, I 3]) (I 3))))
    (InvalidTupleProj 3 3)))

-- | Evaluate a tuple projection using a literal w/ dups
testEvalTupProj1 :: Test
testEvalTupProj1 = TestCase (
  assertEqual "Tests that tuple projections work for tuples"
  (Right (Vt [Vi 1, Vi 1, Vi 2, Vi 2, Vi 3, Vi 3]))
  (evalTest (eval $ (Binop Proj (Tuple [I 1, I 2, I 3]) (Tuple [I 0, I 0, I 1, I 1, I 2, I 2])))))

-- | Evaluate a tuple projection using a literal w/ only 2 elements
testEvalTupProj2 :: Test
testEvalTupProj2 = TestCase (
  assertEqual "Tests that tuple projections work for tuples w/ 2 elements only"
  (Right (Vt [Vi 1, Vi 3]))
  (evalTest (eval $ (Binop Proj (Tuple [I 1, I 2, I 3]) (Tuple [I 0,I 2])))))

-- | Evaluate a tuple projection that is out of bounds negative
testEvalTupProjOutOfBounds1 :: Test
testEvalTupProjOutOfBounds1 = TestCase (
  assertBool "Tests that tuple projection fails when below zero"
  (matchesRuntimeError
    (evalTest (eval $ (Binop Proj (Tuple [I 1, I 2, I 3]) (Tuple [I 1, I $ -1, I 0]))))
    (InvalidTupleProj (-1) 3)))

-- | Evaluate a tuple projection that is out of bounds positive
testEvalTupProjOutOfBounds2 :: Test
testEvalTupProjOutOfBounds2 = TestCase (
  assertBool "Tests that tuple projection fails when out of bounds"
  (matchesRuntimeError
    (evalTest (eval $ (Binop Proj (Tuple [I 1, I 2, I 3]) (Tuple [I 1, I 3, I 2]))))
    (InvalidTupleProj 3 3)))
