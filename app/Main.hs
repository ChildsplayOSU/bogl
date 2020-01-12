module Main where
import Runtime.Repl
import System.Environment
-- nothing
main :: IO ()
main = do
  f <- getArgs
  case f of
    [file] -> runFile file
    _ -> putStrLn "USAGE: spiel <filename.bgl>"
