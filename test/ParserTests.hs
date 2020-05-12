module ParserTests(parserTests, checkParseAllExamples) where

--
-- Parser Tests
--

import Test.HUnit
import Parser.Parser
import Language.Syntax
import Language.Types
import qualified Data.Set as S
import Text.Parsec.Error
import System.Directory
import System.FilePath
import Data.Either
import Text.Parsec
--
-- exported tests for the Parser
--
parserTests :: Test
parserTests = TestList [
  testCheckLeft,
  testCheckLeft2,
  testCheckUpdatedBoard,
  testCheckUpdatedBoard2,
  testRejectBadExprAfterSuccessefulParse,
  testParseShortDecl,
  testParseBoardDecl,
  testParseTypeSynAndDecl,
  testNoRepeatedParamNames,
  testNoRepeatedMetaVars,
  testDoubleBoardDeclarations,
  testInvalidBoardDeclaration
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
  (parseAll xtype "" "(Board, (Int, Int))" == Right (Tup [X Board S.empty, Tup [X Itype S.empty, X Itype S.empty]])))


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



testRejectBadExprAfterSuccessefulParse :: Test
testRejectBadExprAfterSuccessefulParse = TestCase (
  assertEqual "Check that the parser does not silently accept bad inputs after a successful parse"
  True
  (isLeft $ parseLine' expr "40 + 2life,the universe, and everything"))

{-- (vestige from old tests)
ex2 = "outcome : (Board,Player) -> Player|Tie \
\ outcome(b,p) = if inARow(3,A,b) then A else \
                \ if inARow(3,B,b) then B else \
                \ if isFull(b)     then Tie"
--}

-- | Hello Int declaration
ex0 :: String
ex0 = "hello : Int\nhello = 24"

-- |Tests parsing a simple declaration
testParseShortDecl :: Test
testParseShortDecl = TestCase (
  assertEqual("Testing parsing of a short declaration")
  True
  (isRight $ parseAll valdef "" ex0)
  )

-- | Simple board decl
ex1 :: String
ex1 = "b : Board\nb!(x,y)=0\nb!(1,1)=1"

-- |Tests parsing a board decl
testParseBoardDecl :: Test
testParseBoardDecl = TestCase (
  assertEqual "Testing parsing of board declaration"
  True
  (isRight $ parseAll valdef "" ex1)
  )

-- | type syn followed by var decl
ex4 :: String
ex4 = "type TestType = {AnotherType}\ntestThis : TestType\ntestThis = AnotherType"

-- |Tests parsing a type declaration followed by variable declaration
testParseTypeSynAndDecl :: Test
testParseTypeSynAndDecl = TestCase (
  assertEqual("Testing parsing of a type synononym followed by a declaration")
  True
  (isRight $ parseAll (typesyn *> many decl) "" ex4)
  )

  {-- (vestige from old tests)
  (() <$ parseAll valdef "" ex1 ==
    Right (Val (Sig "isValid" (Function
      (Ft (Tup [X Board S.empty, X Position S.empty]) (X Booltype S.empty))
    )) (Feq "isValid" (Pars ["b", "p"])
    (If (Binop Equiv (App "b" [Ref "p"]) (S "Empty")) (B True) (B False))) ())))
    --}
-- parsing is a nightmare with annotations...


-- relative to top-level spiel directory
examplesPath = "examples/"
tutorialsPath = examplesPath ++ "tutorials/"

-- | Check whether all
checkParseAllExamples :: IO Bool
checkParseAllExamples = do
    exampleFiles  <- listDirectory examplesPath
    tutorialFiles <- listDirectory tutorialsPath
    let fullPaths = (map ((++) examplesPath) exampleFiles) ++ (map ((++) tutorialsPath) tutorialFiles)
    let bglFiles  = filter (isExtensionOf ".bgl") (fullPaths)
    putStrLn $ "\n***Parsing***\n"
    mapM putStrLn bglFiles
    results <- mapM parseGameFile bglFiles
    let failures = lefts results
    putStrLn $ "\n***Failures:***"
    mapM (putStrLn . (++) "\n" . show) failures
    return $ null failures

testNoRepeatedParamNames :: Test
testNoRepeatedParamNames = TestCase $
   assertEqual "Test parse error on repeated params"
   True
   (isLeft $ parseLine' equation ("foo(a,a) = a + a"))

testNoRepeatedMetaVars :: Test
testNoRepeatedMetaVars = TestCase $
   assertEqual "Test fail on repeated metavariables"
   True
   (isLeft $ parseLine' (boardeqn "myBoard") ("myBoard!(x,x) = Empty"))

-- | Double board decl
ex2 :: String
ex2 = "b : Board\nb!(x,y)=0\nb!(1,1)=1\nb2 : Board\nb2!(x,y)=0"

-- |Tests one board declaration after another with a similar name
testDoubleBoardDeclarations :: Test
testDoubleBoardDeclarations = TestCase (
  assertEqual "Test valid double board declarations"
  True
  (isRight $ parseAll (many decl) "" ex2)
  )

-- | Single bad board decl
ex3 :: String
ex3 = "b : Board\nb!(x,y)=0\nb2!(1,1)=1\nb!(0,0)=1"

-- |Tests a bad board declaration, with an incorrect name in the middle
testInvalidBoardDeclaration :: Test
testInvalidBoardDeclaration = TestCase (
  assertEqual "Test valid double board declarations"
  False
  (isRight $ parseAll (many decl) "" ex3)
  )
