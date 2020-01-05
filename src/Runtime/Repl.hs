-- |

module Runtime.Repl where

import Runtime.Eval
import Parser.Parser
import Text.Parsec
import Language.Syntax
repl :: Game -> IO ()
repl g@(Game _ _ _ vs)= do
  x <- getLine >>= parseLine
  case x of
    Just e -> run (bindings vs) e >> repl g
    Nothing -> repl g
