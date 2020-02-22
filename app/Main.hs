module Main where
import GUI.RapidPrototype
import System.Environment
-- nothing
main :: IO ()
main = do
  f <- getArgs
  case f of
    [file] -> runPrototype file
    _ -> putStrLn "USAGE: spiel <filename.bgl>"
