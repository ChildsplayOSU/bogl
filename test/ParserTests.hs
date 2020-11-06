module ParserTests(parserTests, checkParseAllExamples, ParserTestResult(), parsePassed) where

--
-- Parser Tests
--

import Test.HUnit
import Parser.Parser
import Language.Types
import qualified Data.Set as S
import Data.Either
import Text.Parsec
import Utils
import Utils.String

-- | Checks that all parse results satisfy a predicate
checkAllParse :: Foldable t => (Either ParseError a -> Bool) -> Parser a -> t String -> Bool
checkAllParse prd pr = all (prd . parseAll pr "")

-- | Checks that all parse results are failures
checkAllParseFail :: Foldable t => Parser a -> t String -> Bool
checkAllParseFail = checkAllParse isLeft

-- | Checks that all parse results are successes
checkAllParsePass :: Foldable t => Parser a -> t String -> Bool
checkAllParsePass = checkAllParse isRight

--
-- exported tests for the Parser
--
parserTests :: Test
parserTests = TestList [
  parseTypeTests,
  parseDeclTests,
  testParseRawPreludeAndGamefile,
  parsePreludeTests,
  parseBoardTests,
  parseGameNameTests,
  testDivByZeroBad,
  testUnderscoresInTypes,
  testProperTypeSharing,
  testOptionalBoardInputTests,
  testTypeExtSyn1,
  testTypeExtSyn2,
  testTypeSynCannotBeItsOwnValue,
  testTypeExtLimitation1, -- todo: remove when this becomes a type error
  testTypeExtLimitation2, -- todo: remove when this becomes a type error
  testIdentifiersMustBeLower,
  testNestedExprInWhileOkay,
  testIllFormedLiteral,
  testWellFormedLiteral,
  testRejectReservedNameSymbol,
  testMisnamedDefIsParseError1,
  testMisnamedDefIsParseError2,
  testMisnamedDefIsParseError3,
  testMisnamedDefWithArgsIsParseError,
  testCasings -- casing test group
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
-- Parse Type Tests
--
--
parseTypeTests :: Test
parseTypeTests = TestLabel "Parse Type Tests" (TestList [
  testCheckLeft,
  testCheckLeft2,
  testFunctionType1,
  testFunctionType2,
  testFunctionType3,
  testFunctionType4,
  testRejectSingletonTupleType
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

testFunctionType1 :: Test
testFunctionType1 = TestCase (
  assertEqual "Function non tuple types"
  (Right (Ft intxt intxt))
  (parseAll ftype "" "Int -> Int"))

testFunctionType2 :: Test
testFunctionType2 = TestCase (
  assertEqual "Function tuple types"
  (Right (Ft (Tup [Tup [intxt, intxt], boolxt]) (Tup [intxt, intxt])))
  (parseAll ftype "" "((Int, Int), Bool) -> (Int, Int)"))

testFunctionType3 :: Test
testFunctionType3 = TestCase (
  assertEqual "Function tuple types"
  (Right (Ft (Tup [boardxt, boardxt]) intxt))
  (parseAll ftype "" "(Board, Board) -> Int"))

testFunctionType4 :: Test
testFunctionType4 = TestCase (
  assertEqual "Function tuple types"
  (Right (Ft boolxt (Tup [boardxt, boardxt])))
  (parseAll ftype "" "Bool -> (Board, Board)"))

testRejectSingletonTupleType :: Test
testRejectSingletonTupleType = TestCase (
  assertBool "Reject singleton tuple types"
  (isLeft $ parseAll ftype "" "(Board) -> Int"))

--
--
-- Parse Declaration Tests
--
--
parseDeclTests :: Test
parseDeclTests = TestLabel "Parse Declaration Tests" (TestList [
  testRejectSignatureDecl1,
  testRejectSignatureDecl2,
  testRejectBadExprAfterSuccessefulParse,
  testParseShortDecl,
  testParseBoardVal,
  testParseMod,
  testParseTypeSynAndDecl,
  testNoRepeatedParamNames,
  testNoRepeatedMetaVars,
  testAnySymbolDisallowed,
  testLowerCaseTypeNamesDisallowed_inDecl,
  testLowerCaseTypeNamesDisallowed_inGame
  ])


testRejectSignatureDecl1 :: Test
testRejectSignatureDecl1 = TestCase (
  assertEqual("Rejecting a type declaration in a type signature")
  True
  (isLeft $ parseAll valdef "" "a : Int & {Nothing} \na = 1")
  )

testRejectSignatureDecl2 :: Test
testRejectSignatureDecl2 = TestCase (
  assertEqual("Rejecting a type declaration in a type signature")
  True
  (isLeft $ parseAll valdef "" "f : {A} -> Int \nf(a) = 1")
  )

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

-- | Tests parsing a board value equation
testParseBoardVal :: Test
testParseBoardVal = TestCase (
  assertEqual("Parsing a board value equation")
  True
  (isRight $ parseAll valdef "" "b2 : Board\nb2 = b1")
  )

-- | Tests that infix mod '%' can be parsed correctly
testParseMod :: Test
testParseMod = TestCase (
  assertEqual "Test that the parser recognizes '%' as infix mod"
  True
  (isRight $ (parseAll valdef "" "test:Int\ntest = 5 % 2")))


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
  (isLeft $ parseLine' (equation "foo") ("foo(a,a) = a + a"))


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


-- | Tests that lowercase letters cannot start a type syn name (in decl)
testLowerCaseTypeNamesDisallowed_inDecl :: Test
testLowerCaseTypeNamesDisallowed_inDecl = TestCase (
  assertEqual "Tests that type syns can't begin with a lowercase character"
  False
  (isRight $ parseAll (many decl) "" "type oops={A,B}"))


-- | Tests that lowercase letters cannot start a type syn name (in typesyn)
testLowerCaseTypeNamesDisallowed_inGame :: Test
testLowerCaseTypeNamesDisallowed_inGame = TestCase (
  assertEqual "Tests that type syns can't begin with a lowercase character"
  False
  (isRight $ parseAll (parseGame []) "" "game E\ntype oops={A,B}"))


--
--
-- Parse all Examples
--
--

-- | Represents the rest result for typchecking examples
-- On Fail, return # failures for easier analysis in the cmd line
data ParserTestResult =
  Fail Int |
  Pass

-- | Allows displaying of a parser example checking result
instance Show ParserTestResult where
  show (Fail fails) = "Parsing all Examples\n Failures: " ++ show fails
  show Pass         = "Parsing all Examples\n Failures: 0\n (Passed)"


-- | Determines whether a type checker result is passing or not
parsePassed :: ParserTestResult -> Bool
parsePassed (Fail _ ) = False
parsePassed Pass      = True


-- | Check whether all examples pass parsing
checkParseAllExamples :: IO ParserTestResult
checkParseAllExamples = do
    bglFiles <- getExampleFiles
    logTestStmt "Parsing:"
    mapM_ (putStrLn . ("\t" ++)) bglFiles
    results <- mapM parseGameFile bglFiles
    let allfailures = lefts results
    logTestStmt "Failures:"
    mapM_ (putStrLn . (++) "\n" . show) allfailures
    let failCount = length allfailures
    return $ if failCount < 1 then Pass else (Fail failCount)


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
    Right valdefs -> isRight $ parseGameFromText rawGamecode "Program" valdefs
    Left _        -> False
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
  testUnderscoreInGameNameGood,
  testRejectReservedGameName
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

-- | Tests that board game names that are reserved words are not allowed
testRejectReservedGameName :: Test
testRejectReservedGameName = TestCase (
  assertEqual "Rejects game names that are reserved words"
  True
  $ checkAllParseFail (parseGame []) gs
  )
  where
     gs = map ("game " ++) reservedNames

-- | Dividing by zero turns up a Right Err, as long as this doesn't crash due to an exception it is likely that it worked
testDivByZeroBad :: Test
testDivByZeroBad = TestCase (
  assertEqual "Test that div by zero is not allowed"
  True
  (isRight $ parseAll (many decl) "" "f : Int\nf = 1/0"))


-- | Test parsing types with underscores
testUnderscoresInTypes :: Test
testUnderscoresInTypes = TestCase (
  assertEqual "Tests that types allow underscores in them"
  True
  (isRight $ parseAll (parseGame []) "" "game E\ntype Board=Array(1,1) of Int\ntype Input=Int\ntype Under_Type={U_1,U_2,U3_24A}"))

-- | Simple game header to use in tests
sg :: String
sg = "game G\ntype Board=Array(1,1) of Int\ntype Input=Int\n"


-- | Tests that types declared in the code are available in the prelude, and vice versa
-- at the appropriate points
testProperTypeSharing :: Test
testProperTypeSharing = TestCase (
  assertEqual "Tests that types are properly shared amongst a prelude & gamefile"
  True
  (case parsePreludeFromText "type Prelude_Type={A,B}" of
    Right valdefs -> isRight $ parseGameFromText (sg ++ "f:Prelude_Type\nf=A") "Program" valdefs
    Left _        -> False))


--
-- Optional Board/Input Tests
--

testOptionalBoardInputTests :: Test
testOptionalBoardInputTests = TestLabel "Optional Board/Input Tests" (TestList [
  testNoExplicitBoard,
  testNoExplicitInput,
  testNoExplicitBoardOrInput,
  testBadBoardType,
  testBadInputType,
  testNoExplicitBoardIsOfInt,
  testNoExplicitInputIsInt,
  testSimilarTypeToBoardOkay,
  testSimilarTypeToInputOkay
  ])

-- | No explicit Board type should work
testNoExplicitBoard :: Test
testNoExplicitBoard = TestCase (
  assertEqual "Test that no explicit Board type should work"
  True
  (isRight $ parseAll (parseGame []) "" "game E\ntype Input=Int"))


-- | No explicit Input type should work
testNoExplicitInput :: Test
testNoExplicitInput = TestCase (
  assertEqual "Test that no explicit Input type should work"
  True
  (isRight $ parseAll (parseGame []) "" "game E\ntype Board=Array(1,1) of Int"))


-- | No explicit Input or Board type should work
testNoExplicitBoardOrInput :: Test
testNoExplicitBoardOrInput = TestCase (
  assertEqual "Test that no explicit Board or Input type should work"
  True
  (isRight $ parseAll (parseGame []) "" "game E"))


-- | Should still recognize a bad Board type
testBadBoardType :: Test
testBadBoardType = TestCase (
  assertEqual "Test that a bad Board type is caught"
  False
  (isRight $ parseAll (parseGame []) "" "game E\ntype Board = Int"))


-- | Should still recognize a bad Input type
testBadInputType :: Test
testBadInputType = TestCase (
  assertEqual "Test that a bad Input type is caught"
  False
  (isRight $ parseAll (parseGame []) "" "game E\ntype Input = Input"))


-- | No explicit board should work
testNoExplicitBoardIsOfInt :: Test
testNoExplicitBoardIsOfInt = TestCase (
  assertEqual "Test that no explicit Board is of type Int"
  True
  (isRight $ parseAll (parseGame []) "" "game E\ntype Input=Int\nb:Board\nb!(x,y)=1"))


-- | No explicit input should work
testNoExplicitInputIsInt :: Test
testNoExplicitInputIsInt = TestCase (
  assertEqual "Test that no explicit Input is of type Int"
  True
  (isRight $ parseAll (parseGame []) "" "game E\nf:Int->Int\nf(q)=let x = input in x"))

-- | Boards should be okay where a normal Board would be
testSimilarTypeToBoardOkay :: Test
testSimilarTypeToBoardOkay = TestCase (
  assertEqual "Test that type synonym 'Boards' isn't mixed up with 'Board'"
  True
  (isRight $ parseAll (parseGame []) "" "game E\ntype Boards=Int"))

-- | Inputs should be okay where a normal Input would be
testSimilarTypeToInputOkay :: Test
testSimilarTypeToInputOkay = TestCase (
  assertEqual "Test that type synonym 'Inputs' isn't mixed up with 'Input'"
  True
  (isRight $ parseAll (parseGame []) "" "game E\ntype Inputs=Int"))

-- | Tests that 'type AB = {AB}' is not a valid type syn
-- A type syn cannot be a value of itself
testTypeSynCannotBeItsOwnValue :: Test
testTypeSynCannotBeItsOwnValue = TestCase (
  assertEqual "Test that a type syn cannot be listed as one of it's own symbols"
  False
  (isRight $ parseAll (parseGame []) "" "game E\ntype AB={AB}"))

testTypeExtSyn1 :: Test
testTypeExtSyn1 = TestCase (
  assertEqual "Test that a type can be extended by an etype synonym"
  True
  (isRight $ parseAll (parseGame []) "" "game E\ntype T1 = {A}\ntype T2 = Int & T1"))

testTypeExtSyn2 :: Test
testTypeExtSyn2 = TestCase (
  assertEqual "Test that a type can be extended by an etype synonym"
  True
  (isRight $ parseAll (parseGame []) "" "game E\ntype T1 = {A}\ntype T2 = T1 & {B}"))

-- todo: remove the testTypeExtLimitation tests when it is moved to the type checker
testTypeExtLimitation1 :: Test
testTypeExtLimitation1 = TestCase (
  assertEqual "Test that a type cannot be extended by a non etype"
  False
  (isRight $ parseAll (parseGame []) "" "game E\ntype T1 = Int & {A}\ntype T2 = Int & T1"))

testTypeExtLimitation2 :: Test
testTypeExtLimitation2 = TestCase (
  assertEqual "Test that a type cannot be extended by a non etype"
  False
  (isRight $ parseAll (parseGame []) "" "game E\ntype T1 = Board\ntype T2 = Int & T1"))

-- | Tests that identifiers must starst with a lowercase alpha char
testIdentifiersMustBeLower :: Test
testIdentifiersMustBeLower = TestCase (
  assertEqual "Tests that identifiers must begin with a lowercase character"
  False
  (isRight $ parseAll (many decl) "" "F:Int\nF=5\nF2:Int->Int\nF2(x)=x"))

testNestedExprInWhileOkay :: Test
testNestedExprInWhileOkay = TestCase (
  assertEqual "Test that unparenthesized nested expressions are allowed in while"
  True
  (isRight $ parseAll expr "" "while x < 10 do x + 1"))

testWellFormedLiteral :: Test
testWellFormedLiteral = TestCase (
  assertEqual "Test that well-formed literals parse"
  True $
  checkAllParsePass literal lits)
     where
        lits = ["1", "True", "False", "-1", "+1", "A", "(((40, 2), Nested, Tuple), 0)",
                "(Parenthesized)", "       (Whitespace      ,     100    )"]

testIllFormedLiteral :: Test
testIllFormedLiteral = TestCase (
  assertEqual "Test that ill-formed literals do not parse"
  True $
  checkAllParseFail literal lits)
     where
        lits = ["1 +", "1 * 1", "input", "let x = 1 in while x < 10 do x + 1", "1,1", "(1,)",
                "(1,,1)", "()", "(,1,2)", "-- 1", "{- 1 -}"]

-- | Tests that reserved names are not valid symbols
testRejectReservedNameSymbol :: Test
testRejectReservedNameSymbol = TestCase (
  assertEqual "Rejects symbols that are reserved words"
  True
  $ checkAllParseFail enum $ map (surrounds "{" "}") reservedNames
  )

-- | Tests that a longer equation name is caught
testMisnamedDefIsParseError1 :: Test
testMisnamedDefIsParseError1 = TestCase (
  assertEqual "Test that a misnamed val definition triggers a parse error"
  False
  (isRight $ parseAll (many decl) "" "b:Int\nb2=2"))

-- | Tests that a shorter equation name is caught
testMisnamedDefIsParseError2 :: Test
testMisnamedDefIsParseError2 = TestCase (
  assertEqual "Test that a misnamed val definition triggers a parse error"
  False
  (isRight $ parseAll (many decl) "" "b2:Int\nb=2"))

-- | Tests that a mispelled equation name is caught
testMisnamedDefIsParseError3 :: Test
testMisnamedDefIsParseError3 = TestCase (
  assertEqual "Test that a misnamed val definition triggers a parse error"
  False
  (isRight $ parseAll (many decl) "" "t:Int\na=2"))

-- | Tests that a mispelled equation name is caught
testMisnamedDefWithArgsIsParseError :: Test
testMisnamedDefWithArgsIsParseError = TestCase (
  assertEqual "Test that a misnamed func definition triggers a parse error"
  False
  (isRight $ parseAll (many decl) "" "t:Int -> Int\na(x)=x+1"))

--
-- Casing tests
--
testCasings :: Test
testCasings = TestLabel "Casing Tests" (TestList [
  testUppercaseParamsFail,
  testUppercaseFunctionApplicationFails,
  testUppercaseLetNameFails
  ])

-- | Tests that uppercase params fail to parse
testUppercaseParamsFail :: Test
testUppercaseParamsFail = TestCase (
  assertEqual "Tests that uppercase params fail to parse"
  False
  (isRight $ parseAll (parseGame []) "" "game E\ntype T={X,O}\nf:T -> Bool\nf(X)=if X==O then True else False"))

-- | Tests that uppercase params fail to parse
testUppercaseFunctionApplicationFails :: Test
testUppercaseFunctionApplicationFails = TestCase (
  assertEqual "Tests that uppercase function application fails to parse"
  False
  (isRight $ parseAll (parseGame []) "" "game E\ntype T={X,O}\nf:T\nf=X(True)"))

-- | Tests that uppercase params fail to parse
testUppercaseLetNameFails :: Test
testUppercaseLetNameFails = TestCase (
  assertEqual "Tests that uppercase let params fail to parse"
  False
  (isRight $ parseAll (parseGame []) "" "game E\ntype T={X,O}\nf:Int\nf=let X=5 in 5"))
