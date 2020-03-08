module ParserTests(parserTests) where

--
-- Parser Tests
--

import Test.HUnit
import Parser.Parser
import Language.Syntax
import Language.Types
import qualified Data.Set as S
import Text.Parsec.Error

--
-- exported tests for the Parser
--
parserTests :: Test
parserTests = TestList [
  testCheckLeft,
  testCheckLeft2,
  testCheckUpdatedBoard,
  testCheckUpdatedBoard2,
  testParseBinaryOp,
  testRejectBadExprAfterSuccessefulParse,
  testParseLongExpr,
  testBoardeqn,
  testNoRepeatedParamNames, 
  testParseEqn, 
  testNoRepeatedMetaVars
  ]

--
-- TEST STRUCTURE
--
-- testNameOfTest :: Test
-- testNameOfTest = TestCase (
--  assertEqual "Test Description"
--  ExpectedValue
--  ExpressionToCheck)
--

-- check left arg
testCheckLeft :: Test
testCheckLeft = TestCase (
  assertEqual "Parser Check Left"
  True
  (isLeft $ parseAll xtype "" "(3)"))

testCheckLeft2 :: Test
testCheckLeft2 = TestCase (
  assertEqual "Parser Check Left 2"
  True
  (isLeft $ parseAll xtype "" "(22222)"))

-- updating board tests
testCheckUpdatedBoard :: Test
testCheckUpdatedBoard = TestCase (
  assertEqual "Check Updated Board"
  True
  (parseAll xtype "" "(Board, Position)" == Right (Tup [X Board S.empty, X Position S.empty])))


-- TODO used to be --"Right (Symbol(no extension),Board(no extension))"
-- determine whether a symbol and board can be evaluated
testCheckUpdatedBoard2 :: Test
testCheckUpdatedBoard2 = TestCase (
  assertEqual "Check Updated Board 2"
  (Right $ Tup [(X Top (S.fromList ["X"])), X Board S.empty])
  (parseAll xtype "" "({X}, Board)"))


-- | Read a single line and return the result (intended for brevity in test cases)
parseLine' :: Parser a -> String -> Either ParseError a
parseLine' pars = parseAll pars ""

-- test binary op functionality being parsed & evaluated
testParseBinaryOp :: Test
testParseBinaryOp = TestCase (
  assertEqual "Check Parsing of Binary Op"
  True
  (parseLine' expr "40 + 2" == Right (Binop Plus (I 40) (I 2))))


testRejectBadExprAfterSuccessefulParse :: Test
testRejectBadExprAfterSuccessefulParse = TestCase (
  assertEqual "Check that the parser does not silently accept bad inputs after a successful parse"
  True
  (isLeft $ parseLine' expr "40 + 2life,the universe, and everything"))

-- |
ex1 :: String
ex1 = "isValid : (Board, Position) -> Bool\n  isValid(b,p) = if b(p) == Empty then True else False"

{--
ex2 = "outcome : (Board,Player) -> Player|Tie \
\ outcome(b,p) = if inARow(3,A,b) then A else \
                \ if inARow(3,B,b) then B else \
                \ if isFull(b)     then Tie"
--}

-- testing long expression
testParseLongExpr :: Test
testParseLongExpr = TestCase (
  assertEqual "Testing parsing of long expression"
  True
  (parseAll valdef "" ex1 ==
    Right (Val (Sig "isValid" (Function
      (Ft (Tup [X Board S.empty, X Position S.empty]) (X Booltype S.empty))
    )) (Feq "isValid" (Pars ["b", "p"])
    (If (Binop Equiv (App "b" [Ref "p"]) (S "Empty")) (B True) (B False))))))


{--
checkGameParse :: IO (Maybe Game) -> IO Bool
checkGameParse a = do
    q <- a
    case q of
      Just _  -> return (True)
      Nothing -> return (False)

-- tests the initial example file we have
testExampleFile1 :: Test
testExampleFile1 = TestCase (
  assertBool "Tests that example file 1 can be loaded"
  (checkGameParse (parseGameFile "../examples/example1.bgl")))
--}

testParseEqn :: Test
testParseEqn = TestCase $ 
   assertEqual "Test parse well-formed equation"
   True 
   (parseLine' equation "add(a,b) = a + b" == Right (Feq "add" (Pars  ["a", "b"]) (Binop Plus (Ref "a") (Ref "b"))))
     
testNoRepeatedParamNames :: Test 
testNoRepeatedParamNames = TestCase $  
   assertEqual "Test parse error on repeated params" 
   True 
   (isLeft $ parseLine' equation ("foo(a,a) = a + a"))
   
testBoardeqn :: Test 
testBoardeqn = TestCase $  
   assertEqual "Test board equation parse" 
   True 
   (parseLine' (boardeqn "myBoard") ("myBoard!(x,y) = Empty") == Right (PosDef "myBoard" (ForAll "x") (ForAll "y") (S "Empty")))
   
testNoRepeatedMetaVars :: Test 
testNoRepeatedMetaVars = TestCase $  
   assertEqual "Test fail on repeated metavariables" 
   True 
   (isLeft $ parseLine' (boardeqn "myBoard") ("myBoard!(x,x) = Empty"))   
