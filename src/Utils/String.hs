{-|
Description : Utility functions for displaying strings
-}

module Utils.String where

import Data.List

-- Prepend l to r and append b
surrounds :: [a] -> [a] -> [a] -> [a]
surrounds l r b = l ++ b ++ r

-- | Prepend and append a to b
surround :: [a] -> [a] -> [a]
surround a b = surrounds a a b

-- | Insert b between each lr-surrounded element of a list
surroundMany :: [a] -> [a] -> [a] -> [[a]] -> [a]
surroundMany l b r ss = ((intercalate b . map (surrounds l r)) ss)

-- | Double quotes a string
quote :: String -> String
quote = surround "\""

-- | Double quote and append a comma to each string in a list
quoteMany :: [String] -> String
quoteMany = surroundMany "\"" ", " "\""

-- | Display a list as a tuple
showAsTuple :: [String] -> String
showAsTuple = surrounds "(" ")" . intercalate ", "

-- | Surround a string with parenthesis
parenthesize :: String -> String
parenthesize = surrounds "(" ")"
