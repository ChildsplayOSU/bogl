module Main where

import System.Environment
import API.Run
import API.JSONData
import Runtime.Values
import Data.Array

-- | Entry point to run the command line interface (interpreter/repl)
main :: IO ()
main = do
  putStrLn "=============================="
  putStrLn "BoGL (The Board Game Language)"
  putStrLn "Created at the School of EECS"
  putStrLn "Oregon State University"
  putStrLn "Version 1.2.0"
  putStrLn "=============================="
  args <- getArgs
  -- attempt to load up this bogl file
  case args of
    -- at least one arg, treat as a file name to load
    (fn:_) -> putStrLn ("* loading " ++ fn) >> load fn
    -- no args, run standalone repl only
    []     -> putStrLn "* running repl" >> repl

-- | Load a BoGL file for the repl
load :: String -> IO ()
load fn = do
  c <- readFile fn
  -- evaluate this file
  let sc = SpielCommand "" c "1" [] fn
  let rez = _runCodeWithCommands sc
  case rez of
    -- valid case, run REPL
    -- types followed by value means we're good to go
    -- but we ignore the first list of types
    (_:(SpielValue _ _):[]) -> do
      putStrLn $ "* parsed & typechecked " ++ fn
      putStrLn "* Running BoGL Repl"
      runRepl sc -- run the full repl
    _                     -> do
      mapM (\r -> putStrLn $ showResponse r) rez
      return ()

-- | Run the repl by itself (no file loaded)
repl :: IO ()
repl = runRepl $ SpielCommand "" "game BoglProgram" "1" [] "Interpreter"

-- | Runs the REPL for Bogl
runRepl :: SpielCommand -> IO ()
runRepl (SpielCommand p f _ b n) = do
  putStrLn ""
  putStrLn ">>> "
  ln <- getLine
  putStrLn ""
  case ln of
    -- various exit forms
    ":q"    -> return ()
    "exit"  -> return ()
    "quit"  -> return ()
    -- reload the current file
    ":r"    -> do
      putStrLn $ "* reloading " ++ n
      case n of
        -- no reload, running w/out a file
        "Interpreter" -> runRepl $ SpielCommand p "game BoglProgram" "" b n
        -- reload normally
        _             -> do
          newFileContent <- readFile n
          runRepl $ SpielCommand p newFileContent "" b n
    -- evaluate this expression
    input'  -> do
      let sc = SpielCommand p f input' b n
      let rez = _runCodeWithCommands sc
      mapM_ (handleResult sc) rez
      return ()
  where
    -- | handle the result from evaluating a given expr
    handleResult :: SpielCommand -> SpielResponse -> IO ()
    handleResult _ (SpielTypes _) = return ()
    handleResult c p'@(SpielPrompt _) = do
      putStrLn $ showResponse p'
      replPrompt c
      runRepl c
    handleResult c r = do
      putStrLn $ showResponse r
      runRepl c

-- | Evaluate an IO prompt
replPrompt :: SpielCommand -> IO ()
replPrompt (SpielCommand p' f' i' b' n') = do
  putStrLn ""
  putStrLn "*** Please input a value"
  putStrLn "<<< "
  ln <- getLine
  putStrLn ""
  case ln of
    ":q"    -> return ()
    "exit"  -> return ()
    "quit"  -> return ()
    input'  -> do
      let sc = SpielCommand p' f' i' (input':b') n'
      let rez = _runCodeWithCommands sc
      mapM_ (handleResult sc) rez
      return ()
  where
    handleResult :: SpielCommand -> SpielResponse -> IO ()
    handleResult _ (SpielTypes _) = return ()
    handleResult _ g@(SpielGameResult _) = do
      putStrLn $ showResponse g
      return ()
    handleResult _ v@(SpielValue _ _) = do
      putStrLn $ showResponse v
      return ()
    handleResult _ e@(SpielError _) = do
      putStrLn $ showResponse e
      return ()
    handleResult (SpielCommand p f i b n) e@(SpielParseError _ _ _ _) = do
      putStrLn $ showResponse e
      let c' = (SpielCommand p f i (tail b) n)
      replPrompt c'
    handleResult (SpielCommand p f i b n) e@(SpielTypeError _) = do
      putStrLn $ showResponse e
      let c' = (SpielCommand p f i (tail b) n)
      replPrompt c'
    handleResult c r = do
      putStrLn $ showResponse r
      replPrompt c

-- TODO remove this entry
-- | Attempts to extract a filename if one exists in the arg list
--getFileName :: [String] -> String
--getFileName []    = error "Pass a filename to run with BoGL!"
--getFileName (x:_) = x

-- | Print boards onto the screen
showBoards :: [Val] -> String
showBoards [] = ""
showBoards ((Vboard b):ls) = printArr b ++ showBoards ls
  where
    printArr a = let (_,(bx,by)) = bounds a in
                 unlines [unwords [show (a ! (x,y)) | x <- [1..bx]] | y <- [1..by]] ++ "\n"
-- we should always get boards back
showBoards _ = error "Unexpected non-board to print"

-- | Converts a spiel response to an appropriate local response
showResponse :: SpielResponse -> String
showResponse (SpielPrompt bl) = showBoards bl
showResponse (SpielSuccess result) = "OK: " ++ result
showResponse (SpielGameResult gr) = show gr
showResponse (SpielParseError _ _ _ m) = "\nParse Error: " ++ m
showResponse (SpielValue bl v) = showBoards bl ++ " " ++ show v
showResponse (SpielTypeHole m _ _ ) = show m
showResponse (SpielError m) = "\nError: " ++ show m
showResponse (SpielTypeError e) = show e
showResponse (SpielRuntimeError e) = e
-- ignore all other response types, these are meant for the server
showResponse _ = ""
