module ParserTests(parserTests) where

--
-- Parser Tests
--

import Test.HUnit
import Parser.Parser
import Language.Syntax
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
  testCanParseLine,
  testParseLongExpr]

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
  (isLeft $ parseAll ftype "" "(3)"))

testCheckLeft2 :: Test
testCheckLeft2 = TestCase (
  assertEqual "Parser Check Left 2"
  True
  (isLeft $ parseAll ftype "" "(Symbol)"))

-- updating board tests
testCheckUpdatedBoard :: Test
testCheckUpdatedBoard = TestCase (
  assertEqual "Check Updated Board"
  True
  (parseAll ttype "" "(Board, Position)" == Right (Tup [X Board S.empty, X Position S.empty])))


testCheckUpdatedBoard2 :: Test
testCheckUpdatedBoard2 = TestCase (
  assertEqual "Check Updated Board 2"
  "Right (Symbol(no extension),Board(no extension))"
  (show (parseAll ttype "" "(Symbol,Board)")))


-- | Read a single line and return the result (intended for brevity in test cases)
parseLine' :: Parser a -> String -> Either ParseError a
parseLine' p = parseAll p ""

-- test binary op functionality being parsed & evaluated
testParseBinaryOp :: Test
testParseBinaryOp = TestCase (
  assertEqual "Check Parseing of Binary Op"
  True
  (parseLine' expr "40 + 2" == Right (Binop Plus (I 40) (I 2))))


testCanParseLine :: Test
testCanParseLine = TestCase (
  assertEqual "Can parse weird line"
  True
  (isLeft $ parseLine' expr "40 + 2life,the universe, and everything"))

-- |
ex1 :: String
ex1 = "isValid : (Board,Position) -> Bool\n  isValid(b,p) = if b(p) == Empty then True else False"

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
    Right (Val (Sig "isValid" (Function (Ft (Pt (Tup [X Board S.empty, X Position S.empty]))
    (Pext (X Booltype S.empty))))) (Feq "isValid" (Pars ["b", "p"])
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
