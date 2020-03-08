module Parser.ParseError where 

import Data.List 

quote :: String -> String 
quote s = "\"" ++ s ++ "\"" 

quoteMany :: [String] -> String 
quoteMany ss = quote $ intercalate "\", \"" ss

errRepeatParam :: [String] -> String -> String 
errRepeatParam r n = "repeated parameter name " ++ (quoteMany r) ++  
                     " in declaration of " ++ quote n
