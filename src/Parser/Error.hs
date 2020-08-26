{-|
Module      : Parser.Error
Description : Parser Errors
Copyright   : (c)
License     : BSD-3

Holds some functions for reporting parser errors

-}

module Parser.Error where

import Data.List

-- | Double quotes a string
quote :: String -> String
quote s = "\"" ++ s ++ "\""

-- | Uses a list of strings to produce a single string of the double quoted strings
quoteMany :: [String] -> String
quoteMany ss = quote $ intercalate "\", \"" ss

-- | Constructs an error message indicating which params were repeated in what context
errRepeatParam :: [String] -> String -> String
errRepeatParam r n = "repeated parameter name " ++ (quoteMany r) ++
                     " in declaration of " ++ quote n
