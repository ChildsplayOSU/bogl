-- |

module Runtime.Repl where

import Runtime.Eval
import Parser.Parser
import Text.Parsec
import Language.Syntax
import Runtime.Typechecker

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