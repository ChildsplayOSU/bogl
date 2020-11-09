{-|
Description : API tests
-}

module APITests (apiTests) where

import Test.HUnit
import API.Run
import API.JSONData

import Error.TypeError (TypeError(..))
import Error.Error (Error(..), Err(..))

import Runtime.Values

apiTests :: Test
apiTests = TestList [
   testLatestInputHasWrongType,
   testGoodCommand
   ]

-- | Contructs a 'SpielCommand' given a list of input strings
lacksInput :: [String] -> SpielCommand
lacksInput = \b -> SpielCommand "" "game G type Input = Int" "1" b "TestAPI"

-- Note: the tests below are preliminary
-- We should
-- 1) set the API straight (clarify how it should work, remove unused responses, etc)
-- 2) write some helper functions to verify certain properties of the 'SpielResponses'
--    for example, that a certain response is contained in the 'SpielResponses'
--       the SpielTypeError example requires too much pattern matching, for example
--       we should not let that type of code proliferate in these tests
-- 3) write more thorough tests

testLatestInputHasWrongType :: Test
testLatestInputHasWrongType = TestCase $
   assertBool "Test that a type error is produced if the latest input has the wrong type."
   check
   where
      check = case _runCodeWithCommands (lacksInput ["True"]) of
                 [SpielTypeError (Error (TE (InputMismatch _ _ _)) _ _)] -> True
                 _                                                       -> False

testGoodCommand :: Test
testGoodCommand = TestCase $
   assertBool "Test that a good response is returned given a good command"
   check
   where
      check = case _runCodeWithCommands (lacksInput ["1", "2"]) of
                 [_, SpielValue _ (Vi 1)] -> True
                 _                        -> False
