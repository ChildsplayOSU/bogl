module Utils where
--
-- Utils.hs
--
-- Various testing utilities
--

import Runtime.Values
import Runtime.Monad
import Parser.Parser
import Text.Parsec.Error
import System.Directory
import System.FilePath
import Error.RuntimeError

import Language.Types
import Language.Syntax
import Text.Parsec.Pos

-- | The boilerplate for a Game which is to be filled in with ValDefs
testGame :: [ValDef SourcePos] -> Game SourcePos
testGame vd = Game "Test" (BoardDef (5,5) intxt) (InputDef intxt) vd []

-- relative to top-level spiel directory
examplesPath :: String
examplesPath = "examples/"

tutorialsPath :: String
tutorialsPath = examplesPath ++ "tutorials/"

getExampleFiles :: IO [String]
getExampleFiles = do
   exampleFiles  <- listDirectory examplesPath
   tutorialFiles <- listDirectory tutorialsPath
   let fullPaths = (map ((++) examplesPath) exampleFiles) ++ (map ((++) tutorialsPath) tutorialFiles)
       bglFiles  = filter (isExtensionOf ".bgl") (fullPaths)
   return bglFiles

-- used to extract value from expression
evalTest :: Eval Val -> Either Exception Val
evalTest ev = runEval (emptyEnv (0,0)) ([], [], 1) ev

-- Verifies a Value Error (not an exception) was produced during a computation
isRightErr :: Either Exception Val -> Bool
isRightErr m = case m of
                Right (Err _) -> True
                _             -> False

-- | Used to verify an exception matches explicitly, with the provided string message
{-
isExceptionWithString :: Either Exception Val -> String -> Bool
isExceptionWithString m s = case m of
                            Left (Error e) -> trace ("Error was " ++ e) e == s
                            _              -> False
-}

matchesRuntimeError :: Either Exception Val -> RuntimeError -> Bool
matchesRuntimeError m re = case m of
                            Left (Error e) -> show re == e
                            _              -> False

-- | Read a single line and return the result (intended for brevity in test cases)
parseLine' :: Parser a -> String -> Either ParseError a
parseLine' pars = parseAll pars ""

logTestStmt :: String -> IO ()
logTestStmt s = do
    putStrLn $ "\n*** " ++ s ++ " ***\n"
