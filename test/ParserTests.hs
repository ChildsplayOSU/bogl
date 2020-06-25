module ParserTests(parserTests, checkParseAllExamples) where

--
-- Parser Tests
--

import Test.HUnit
import Parser.Parser
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
  parseXTypeTests,
  parseDeclTests,
  testParseRawPreludeAndGamefile,
  parsePreludeTests,
  parseBoardTests,
  parseGameNameTests
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

--
--
-- Parse XType Tests
--
--
parseXTypeTests :: Test
parseXTypeTests = TestLabel "Parse XType Tests" (TestList [
  testCheckLeft,
  testCheckLeft2
  ])


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


--
--
-- Parse Declaration Tests
--
--
parseDeclTests :: Test
parseDeclTests = TestLabel "Parse Declaration Tests" (TestList [
  testRejectBadExprAfterSuccessefulParse,
  testParseShortDecl,
  testParseTypeSynAndDecl,
  testNoRepeatedParamNames,
  testNoRepeatedMetaVars,
  testAnySymbolDisallowed
  ])


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


-- |Tests parsing a simple declaration
testParseShortDecl :: Test
testParseShortDecl = TestCase (
  assertEqual("Testing parsing of a short declaration")
  True
  (isRight $ parseAll valdef "" "hello : Int\nhello = 24")
  )


-- |Tests parsing a type declaration followed by variable declaration
testParseTypeSynAndDecl :: Test
testParseTypeSynAndDecl = TestCase (
  assertEqual("Testing parsing of a type synononym followed by a declaration")
  True
  (isRight $ parseAll (typesyn *> many decl) "" "type TestType = {AnotherType}\ntestThis : TestType\ntestThis = AnotherType")
  )

  {-- (vestige from old tests)
  (() <$ parseAll valdef "" ex1 ==
    Right (Val (Sig "isValid" (Function
      (Ft (Tup [X Board S.empty, X Position S.empty]) (X Booltype S.empty))
    )) (Feq "isValid" (Pars ["b", "p"])
    (If (Binop Equiv (App "b" [Ref "p"]) (S "Empty")) (B True) (B False))) ())))
    --}
-- parsing is a nightmare with annotations...


-- | Tests parsing a decl w/ repeated params
testNoRepeatedParamNames :: Test
testNoRepeatedParamNames = TestCase $
  assertEqual "Test parse error on repeated params"
  True
  (isLeft $ parseLine' equation ("foo(a,a) = a + a"))


-- | Tests parsing a board decl w/ repeated params
testNoRepeatedMetaVars :: Test
testNoRepeatedMetaVars = TestCase $
  assertEqual "Test fail on repeated metavariables"
  True
  (isLeft $ parseLine' (boardeqn "myBoard") ("myBoard!(x,x) = Empty"))


-- | Tests that 'AnySymbol' cannoet be used as a direct type in anything
testAnySymbolDisallowed :: Test
testAnySymbolDisallowed = TestCase (
  assertEqual "Tests that declaring anything of AnySymbol is not allowed"
  False
  (isRight $ parseAll (many decl) "" "f:AnySymbol\nf=X")
  )


--
--
-- Parse all Examples
--
--

-- relative to top-level spiel directory
examplesPath :: String
examplesPath = "examples/"

tutorialsPath :: String
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
    let allfailures = lefts results
    putStrLn $ "\n***Failures:***"
    mapM (putStrLn . (++) "\n" . show) allfailures
    return $ null allfailures


-- | Raw prelude code for the test below
rawPrelude :: String
rawPrelude = "--a prelude\ntestDecl : Int\ntestDecl = 901"
-- | Raw prelude code for the test below
rawGamecode :: String
rawGamecode = "-- a raw gamefile to parse\n\
\game Gamefile\n\
\--setting up the type of the board\n\
\type Board = Array(3,3) of Int\n\
\--setting up the input type\n\
\type Input = Int\n\
\--setting up a type for testing\n\
\type TestType = {A,B}\n\
\--setting up a testing function\n\
\testFunc : TestType -> TestType\n\
\testFunc(t) = if t == A then B else A\n\
\"

-- | Tests parsing a raw prelude and game file from text,
-- without using a file inbetween. This is used for the
-- /runCode endpoint
testParseRawPreludeAndGamefile :: Test
testParseRawPreludeAndGamefile = TestCase $
  assertEqual "Test unable to parse raw prelude and gamefile text"
  True
  (case parsePreludeFromText rawPrelude of
    Right valdefs -> isRight $ parseGameFromText rawGamecode valdefs
    Left err      -> False
  )


--
--
-- Parse Prelude Tests
--
--
parsePreludeTests :: Test
parsePreludeTests = TestLabel "Parse Prelude Tests" (TestList [
  testPreludeStartingWhitespace,
  testPreludeNoStartingWhitespace
  ])


-- | Tests parsing a prelude with starting whitespace
testPreludeStartingWhitespace :: Test
testPreludeStartingWhitespace = TestCase $
   assertEqual "Test fail on prelude w/ comment at the start"
   True
   (isRight $ parsePreludeFromText "--comment at start\nadecl : Int\nadecl = 10\n--another comment\ndecl2 : Int\ndecl2=0")


-- | Tests parsing a prelude w/out starting whitespace
testPreludeNoStartingWhitespace :: Test
testPreludeNoStartingWhitespace = TestCase $
   assertEqual "Test fail on prelude w/out whitespace"
   True
   (isRight $ parsePreludeFromText "ddecl : Int\nddecl = 51\n\n")


--
--
-- Parse Board Tests
--
--
parseBoardTests :: Test
parseBoardTests = TestLabel "Parse Board Tests" (TestList [
  testCheckUpdatedBoard,
  testCheckUpdatedBoard2,
  testParseBoardDecl,
  testDoubleBoardDeclarations,
  testInvalidBoardDeclaration,
  testBoardDimensionsEnforced
  ])


-- | Updating board tests
testCheckUpdatedBoard :: Test
testCheckUpdatedBoard = TestCase (
  assertEqual "Check Updated Board"
  True
  (parseAll xtype "" "(Board, (Int, Int))" == Right (Tup [X Board S.empty, Tup [X Itype S.empty, X Itype S.empty]])))


-- TODO used to be --"Right (Symbol(no extension),Board(no extension))"
-- | Determine whether a symbol and board can be evaluated
testCheckUpdatedBoard2 :: Test
testCheckUpdatedBoard2 = TestCase (
  assertEqual "Check Updated Board 2"
  (Right $ Tup [(X Top (S.fromList ["X"])), X Board S.empty])
  (parseAll xtype "" "({X}, Board)"))


-- | Tests parsing a board decl
testParseBoardDecl :: Test
testParseBoardDecl = TestCase (
  assertEqual "Testing parsing of board declaration"
  True
  (isRight $ parseAll valdef "" "b : Board\nb!(x,y)=0\nb!(1,1)=1")
  )


-- | Tests one board declaration after another with a similar name
testDoubleBoardDeclarations :: Test
testDoubleBoardDeclarations = TestCase (
  assertEqual "Test valid double board declarations"
  True
  (isRight $ parseAll (many decl) "" "b : Board\nb!(x,y)=0\nb!(1,1)=1\nb2 : Board\nb2!(x,y)=0")
  )


-- | Tests a bad board declaration, with an incorrect name in the middle
testInvalidBoardDeclaration :: Test
testInvalidBoardDeclaration = TestCase (
  assertEqual "Test valid double board declarations"
  False
  (isRight $ parseAll (many decl) "" "b : Board\nb!(x,y)=0\nb2!(1,1)=1\nb!(0,0)=1")
  )


-- | Tests that non-zero positive integer board dimensions are enforced
testBoardDimensionsEnforced :: Test
testBoardDimensionsEnforced = TestCase (
  assertEqual "Test that board dimensions are enforced as >= 1"
  False
  (isRight $ parseAll (parseGame []) "" "game G\ntype Board=Array(0,-1) of Int\ntype Input=Int")
  )


--
--
-- Parse Game Name Tests
--
--

parseGameNameTests :: Test
parseGameNameTests = TestLabel "Parse Game Name Tests" (TestList [
  testLowercaseGameNameBad,
  testUppercaseGameNameGood,
  testUnderscoreInGameNameGood
  ])


-- | Tests that game names starting with a lowercase character are disallowed
testLowercaseGameNameBad :: Test
testLowercaseGameNameBad = TestCase (
  assertEqual "Test that starting lowercase game names are disallowed"
  False
  (isRight $ parseAll (parseGame []) "" "game lowercasename\ntype Board = Array(1,1) of Int\ntype Input = Int")
  )


-- | Tests that game names starting with an uppercase character are allowed
testUppercaseGameNameGood :: Test
testUppercaseGameNameGood = TestCase (
  assertEqual "Test that starting uppercase game names are allowed"
  True
  (isRight $ parseAll (parseGame []) "" "game Uppercasename\ntype Board = Array(1,1) of Int\ntype Input = Int")
  )


-- | Tests that board game names with an underscore are allowed
testUnderscoreInGameNameGood :: Test
testUnderscoreInGameNameGood = TestCase (
  assertEqual "Tests that game names with underscores are allowed"
  True
  (isRight $ parseAll (parseGame []) "" "game Ex_Ex_Ex_Ex\ntype Board=Array(1,1) of Int\ntype Input=Int")
  )
