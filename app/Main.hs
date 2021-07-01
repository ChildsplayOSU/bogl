module Main where

import System.Environment
import API.Run
import API.JSONData
import Runtime.Values
import Data.Array

-- nothing
main :: IO ()
main = do
  args <- getArgs
  -- attempt to load up this bogl file
  let fn = getFileName args
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
      repl sc
    _                     -> do
      mapM (\r -> putStrLn $ showResponse r) rez
      return ()

-- | Runs the REPL for Bogl
repl :: SpielCommand -> IO ()
repl (SpielCommand p f _ b n) = do
  putStrLn ""
  putStrLn ">>> "
  ln <- getLine
  putStrLn ""
  case ln of
    ":q"    -> return ()
    "exit"  -> return ()
    "quit"  -> return ()
    input'  -> do
      let sc = SpielCommand p f input' b n
      let rez = _runCodeWithCommands sc
      mapM_ (handleResult sc) rez
      return ()
  where
    handleResult :: SpielCommand -> SpielResponse -> IO ()
    handleResult _ (SpielTypes _) = return ()
    handleResult c p'@(SpielPrompt _) = do
      putStrLn $ showResponse p'
      replPrompt c
      repl c
    handleResult c r = do
      putStrLn $ showResponse r
      repl c

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

-- | Attempts to extract a filename if one exists in the arg list
getFileName :: [String] -> String
getFileName []    = error "Pass a filename to run with BoGL!"
getFileName (x:_) = x

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
