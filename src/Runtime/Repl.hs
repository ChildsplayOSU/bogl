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
      repl' g@(Game n (BoardDef szx szy p) _ vs)= do
        x <- parseLine <$> getLine
        case x of
          Right e -> do
            b <- tcexpr (environment i b vs) e
            if b then runUntilComplete (bindings (szx,szy) vs) e >> repl' g else repl' g
          Left err -> do
            putStrLn (show err)
            repl' g
