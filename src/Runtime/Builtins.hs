{-|
Module      : Runtime.Builtins
Description : Language builtins/Prelude
Copyright   : (c)
License     : BSD-3
-}

module Runtime.Builtins where

import Language.Syntax
import Language.Types

import Runtime.Monad
import Runtime.Values

import qualified Data.Set as S
import Data.Array

import Data.Map ()
import qualified Data.Map as M

import Control.Monad.State

-- | Produces a Tuple type wrapping a single Xtype
single :: Xtype -> Xtype
single x = Tup [x]

-- | List of builtin function signatures
builtinT :: Xtype -> Xtype -> [(String, Type)]
builtinT = \inputT pieceT -> [
  ("input",      Plain $ inputT),
  ("place",      Function (Ft (Tup [pieceT, boardxt, (Tup [intxt, intxt])]) boardxt)),
  ("countBoard", Function (Ft (Tup [pieceT, boardxt]) intxt)),
  ("countCol",   Function (Ft (Tup [pieceT, boardxt]) intxt)),
  ("countRow",   Function (Ft (Tup [pieceT, boardxt]) intxt)),
  ("countDiag",  Function (Ft (Tup [pieceT, boardxt]) intxt)),
  ("isFull",     Function (Ft (single boardxt) boolxt)),
  ("inARow",     Function (Ft (Tup [intxt, pieceT, X Board S.empty]) boolxt)),
  ("not",        Function (Ft (single boolxt) boolxt)),
  ("or",         Function (Ft (Tup [boolxt, boolxt]) boolxt)),
  ("and",        Function (Ft (Tup [boolxt, boolxt]) boolxt))
           ]

-- | places a piece on a board and also adds this new board to the display buffer.
--   We only want the latest version of each unique board, so filter out its predecessor.
place :: [Val] -> Eval Val
place = \[v, Vboard arr, Vt [Vi x, Vi y]] -> do
   let b = Vboard $ arr // [((x,y), v)]
   (tape, boards, iters) <- get
   put (tape, filter (/= Vboard arr) boards ++ [b], iters)
   return b

-- | Verifies the parameters are of the expected number & type from their respective lambdas
-- A final case reports a Runtime error in case anything slips by,
-- which indicates that the number and/or type of parameters didn't match what was expected
-- for that builtin
-- Note: This prevents a crash and gives somewhat better feedback to users,
-- but this should not normally be reachable
-- if the typechecker verifies the expressions correctly in advance
builtinsChecker :: [Char] -> [Val] -> Eval Val
builtinsChecker "place" [v, Vboard arr, Vt [Vi x, Vi y]] = place [v, Vboard arr, Vt [Vi x, Vi y]]
builtinsChecker "countBoard" [v, Vboard arr] = return $ Vi $ length $ filter (== v) (elems arr)
builtinsChecker "countCol" [v, Vboard arr] = return $ Vi $ countCol arr v
builtinsChecker "countRow" [v, Vboard arr] = return $ Vi $ countRow arr v
builtinsChecker "countDiag" [v, Vboard arr] = return $ Vi $ countDiag arr v
builtinsChecker "isFull" [Vboard arr] = return $ Vb $ all (/= Vs "Empty") $ elems arr
builtinsChecker "inARow" [Vi i, v, Vboard arr] = return $ Vb $ inARow arr v i
builtinsChecker "not" [Vb b] = return $ Vb (not b)
builtinsChecker "or" [Vb a, Vb b] = return $ Vb (a || b)
builtinsChecker "and" [Vb a, Vb b] = return $ Vb (a && b)
-- mismatch case for any builtin
builtinsChecker n _ = return $ Err ("Unexpected parameter(s) to '" ++ n ++
                                    "', one or more may have an incorrect Type.")

-- | List of builtin functions, with definitions for evaluation
builtins :: [(Name, [Val] -> Eval Val)]
builtins = [
  ("place", \x -> builtinsChecker "place" x),
  ("remove", \x -> builtinsChecker "remove" x),
  ("countBoard", \x -> builtinsChecker "countBoard" x),
  ("countCol", \x -> builtinsChecker "countCol" x),
  ("countRow", \x -> builtinsChecker "countRow" x),
  ("countDiag", \x -> builtinsChecker "countDiag" x),
  ("isFull", \x -> builtinsChecker "isFull" x),
  ("inARow", \x -> builtinsChecker "inARow" x),
  ("not", \x -> builtinsChecker "not" x),
  ("or", \x -> builtinsChecker "or" x),
  ("and", \x -> builtinsChecker "and" x)
  ]

-- | Builtin references, just input for now
builtinRefs :: [(Name, Eval Val)]
builtinRefs = [
   ("input", readTape)
   ]

-- | the count of adjacent cells in four directions (above, diagonal @ 10:30, left, diagonal @ 7:30)
type Count = (Int, Int, Int, Int)
-- | Map of the count
type CountMap = M.Map (Int, Int) Count

-- | A safe map lookup function which returns a default value for keys not in the map
peek :: (Int, Int) -> CountMap -> Count
peek q m = case M.lookup q m of
               Nothing   -> (0,0,0,0)
               (Just c)  -> c

-- | Adds a cell into the count map
addCell :: (Int, Int) -> CountMap -> CountMap
addCell c@(x,y) m = M.insert c (top + 1, tdiag + 1, left + 1, bdiag + 1) m
   where
      (top,_,_,_)   = peek (x, y - 1) m
      (_,tdiag,_,_) = peek (x - 1, y - 1) m
      (_,_,left,_)  = peek (x - 1, y) m
      (_,_,_,bdiag) = peek (x - 1, y + 1) m

-- | Checks a cell in the count map
checkCell :: Val -> ((Int, Int), Val) -> CountMap -> CountMap
checkCell v (c,v') m = if v == v' then addCell c m else m

-- | Scans cells downwards by column (the order given by (assocs b) with (x,y) coords)
-- each cell's count is the increment of the counts of the four cells before it
checkCells :: Board -> Val -> [Int]
checkCells b v = maxCount
   where
      maxCount = foldr (\c acc -> update c acc) [0,0,0,0] counts
      update (t,td,l,bd) r = zipWith max [t,td,l,bd] r
      counts = M.elems processedBoard
      processedBoard = foldl (\m c -> checkCell v c m) M.empty (assocs b)

-- | Counts cols, rows, and diagonals in a board
countCol, countRow, countDiag :: Board -> Val -> Int
countCol b v = checkCells b v !! 0
countRow b v = checkCells b v !! 2
countDiag b v = max (checkCells b v !! 1) (checkCells b v !! 3)

-- | checks whether a board has i cells containing v in a row
inARow :: Board -> Val -> Int -> Bool
inARow b v i = maximum (checkCells b v) >= i
