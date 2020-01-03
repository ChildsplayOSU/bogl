import Test.DocTest

main :: IO ()
main = doctest ["-isrc", "src/Language/Syntax.hs", "src/Runtime/Eval.hs", "src/Parser/Parser.hs"] 
