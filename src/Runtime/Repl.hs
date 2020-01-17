-- |

module Runtime.Repl where

import Runtime.Eval
import Parser.Parser
import Text.Parsec
import Language.Syntax
import Runtime.Typechecker

runFile :: String -> IO ()
runFile f = do
  parsed <- parseGameFile f
  case parsed of
    Just g -> do
      check <- tc g
      if check then repl g else return ()
    Nothing -> return ()

-- | run an interactive game
repl :: Game -> IO ()
repl g@(Game n i b _) = do
  putStrLn $ "Game: " ++ n
  repl' g
  where
      repl' g@(Game _ _ _ vs)= do
        x <- (getLine) >>= parseLine
        case x of
          Just e -> do
            b <- tcexpr (environment i b vs) e
            if b then run (bindings vs) e >> repl' g else repl' g
          Nothing -> repl' g

-- repl w/o typechecking. TODO: typecheck the while 
runFileU :: String -> IO ()
runFileU f = do
  parsed <- parseGameFile f
  case parsed of
    Just g -> do
      repl g
    Nothing -> return ()

-- | run an interactive game
replU :: Game -> IO ()
replU g@(Game n i b _) = do
  putStrLn $ "Game: " ++ n
  repl' g
  where
      repl' g@(Game _ _ _ vs)= do
        x <- (getLine) >>= parseLine
        case x of
          Just e -> do
            run (bindings vs) e >> repl' g
          Nothing -> repl' g
