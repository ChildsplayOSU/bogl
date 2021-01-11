{-|
Description : Data type for reporting runtime errors
-}

module Error.RuntimeError where

import Utils.String

data RuntimeError = DivideByZero
                  | InvalidBoardAccess (Int,Int) (Int,Int) -- ?
                  | BadComparison String String
                  | BadNumericalOp String String
                  | UndefinedReference String
                  | InvalidLookup String
                  | StackOverflow -- runtime evaluation stack overflow
                  | UnexpectedEvaluation String -- placeholder until we know these are caught at the type level

instance Show RuntimeError where
  show DivideByZero             = "Cannot divide by zero"
  show (InvalidBoardAccess (x,y) (bx,by)) = p1 ++ p2
    where
       p1 = "Could not access (" ++ show x ++ "," ++ show y ++ ") on the board, " ++
         "this is not a valid space. "
       p2 = if bx == by && bx == 1 then "The board only has one space at (1,1)."
                                   else "The board size is ("++ show bx ++ "," ++ show by ++")."
  show (BadComparison a b)      = "Could not compare " ++ a ++ " to " ++ b
  show (BadNumericalOp a b)     = "Could not do numerical operation on " ++ a ++ " to " ++ b
  show (UndefinedReference s)   = (quote s) ++ " is undefined"
  show (InvalidLookup s)        = s ++ " was not correct when looking it up in the environment."
  show StackOverflow            = "Evaluating your expression was stopped for taking too long or being infinite."
  show (UnexpectedEvaluation s) = s
