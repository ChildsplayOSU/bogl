-- | Language builtins/Prelude

module Runtime.Builtins where

import Language.Syntax
import Language.Types

import Runtime.Monad
import Runtime.Values

import qualified Data.Set as S
import Data.Array

import Data.Map (Map) 
import qualified Data.Map as M  

single x = Tup [x]
builtinT :: Xtype -> [(String, Type)]
builtinT = \i -> [
  ("input", Function (Ft (single (X Board S.empty)) i)),
  ("positions", Plain (X Positions S.empty)),
  ("place", Function (Ft (Tup [(X AnySymbol S.empty), (X Board S.empty), (Tup [X Itype S.empty, X Itype S.empty])]) (X Board S.empty))),
  ("remove", Function (Ft (Tup [(X Board S.empty), (Tup [X Itype S.empty, X Itype S.empty])]) (X Board S.empty))),
  ("inARow", Function (Ft (Tup [X Itype S.empty, X AnySymbol S.empty, X Board S.empty]) (X Booltype S.empty))),
  ("isFull", Function (Ft (single (X Board S.empty)) (X Booltype S.empty))),
  ("next", Function (Ft (single (X Top (S.fromList ["X", "O"]))) (X Top (S.fromList ["X", "O"])))),
  ("not", Function (Ft (single (X Booltype S.empty)) (X Booltype S.empty))), 
  ("or", Function (Ft (Tup [X Booltype S.empty, X Booltype S.empty]) (X Booltype S.empty))),
  ("and", Function (Ft (Tup [X Booltype S.empty, X Booltype S.empty]) (X Booltype S.empty))), 
  ("less", Function (Ft (Tup [X Itype S.empty, X Itype S.empty]) (X Booltype S.empty))) 
  -- place and inARow should be polymorphic over all types instead of over all symbols.
           ]

builtins :: [(Name, [Val] -> Eval Val)]
builtins = [
  ("input", \[v] -> readTape v),
  ("place", \[v, Vboard arr, Vt [Vi x, Vi y]] -> return $ Vboard $ arr // [((x,y), v)]),
  ("remove", \[Vboard arr, Vt [Vi x, Vi y]] -> return $ Vboard $ arr // pure ((x,y), Vs "Empty")),
  ("isFull", \[Vboard arr] -> return $ Vb $ all (/= Vs "Empty") $ elems arr),
  ("inARow", \[Vi i, v, Vboard arr] -> return $ Vb $ inARow arr v i),
  ("next", \[Vs s] -> return $ if s == "X" then Vs "O" else Vs "X"),
  ("not", \[Vb b] -> return $ Vb (not b)),
  ("or", \[Vb a, Vb b] -> return $ Vb (a || b)),
  ("and", \[Vb a, Vb b] -> return $ Vb (a && b)), 
  ("less", \[Vi n, Vi m] -> return $ Vb (n < m))
  ]

builtinRefs :: [(Name, Eval Val)]
builtinRefs = [("positions", (getBounds) >>= \(szx, szy) -> return $ Vt [Vt [Vi x, Vi y] | x <- [1..szx], y <- [1..szy]])]

-- the count of adjacent cells in four directions (above, diagonal @ 10:30, left, diagonal @ 7:30) 
type Count = (Int, Int, Int, Int) 
type CountMap = M.Map (Int, Int) Count  

peek :: (Int, Int) -> CountMap -> Count 
peek p m = case M.lookup p m of 
               Nothing   -> (0,0,0,0)  
               (Just c)  -> c  

addCell :: (Int, Int) -> CountMap -> CountMap 
addCell p@(x,y) m = M.insert p (top + 1, tdiag + 1, left + 1, bdiag + 1) m  
   where 
      (top,_,_,_)   = peek (x, y - 1) m 
      (_,tdiag,_,_) = peek (x - 1, y - 1) m   
      (_,_,left,_)  = peek (x - 1, y) m 
      (_,_,_,bdiag) = peek (x - 1, y + 1) m 

checkCell :: Val -> ((Int, Int), Val) -> CountMap -> CountMap 
checkCell v (p,v') m = if v == v' then addCell p m else m 

-- scans cells downwards by column (the order given by (assocs b) with (x,y) coords) 
-- each cell's count is the increment of the counts of the four cells before it 
checkCells :: Board -> Val -> Int 
checkCells b v = maxCount   
   where
      maxCount = foldr (\c m -> let max' = getMaxCount c in if max' > m then max' else m) 0 counts  
      getMaxCount (l,ld,t,rd) = maximum [l,ld,t,rd] 
      counts = M.elems processedBoard 
      processedBoard = foldl (\m c -> checkCell v c m) M.empty (assocs b)  

-- | checks whether a board has i cells containing v in a row 
inARow :: Board -> Val -> Int -> Bool 
inARow b v i = checkCells b v >= i  
