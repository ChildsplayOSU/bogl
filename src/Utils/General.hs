{-|
Description : General utility functions
-}

module Utils.General where

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just $ last xs
