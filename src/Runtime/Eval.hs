--- | Interpreter for Spiel

module Runtime.Eval where
import Language.Syntax
import Language.Types

import Runtime.Values
import Runtime.Monad
import Runtime.Builtins

import Control.Monad
import Data.Array
import Data.List
import Data.Either

import Control.Monad.Writer
import Control.Monad.State

import Text.Parsec.Pos

import Debug.Trace

-- | Produce all of the bindings from a list of value definitions.
--   This is done sequentially. Errors are reported as they're found.
bindings :: (Int, Int) -> [ValDef a] -> Writer [Exception] Env
bindings sz vs = e
  where
    e = foldM (\env (n, v) -> case runEval env ([], [], 1) v of
                                Right v' -> return $ modifyEval ((n, v'):) env
                                Left err -> (tell [err]) >> return env)
        (emptyEnv sz)
        (map bind vs)

bindings_ :: (Int, Int) -> [ValDef a] -> Env
bindings_ x y = (fst . runWriter) (bindings x y)

bind :: (ValDef a) -> (Name, Eval Val)
bind (Val _ (Veq n e) _) = do
  (n, do
    env <- getEnv
    -- bind as a Pending Value
    -- this allows lazy evaluation of vals that use input at runtime
    return $ Pv env (clearAnn e))

bind  (Val _ (Feq n (Pars ls) e) _) = (n, do
                                        env <- getEnv
                                        return $ Vf ls env (clearAnn e))
bind (BVal (Sig n _) defs _) = (n, do
      sz <- getBounds
      values <- mapM (eval . boardExpr) defs
      maybeBoard <- lookupName n
      case maybeBoard of
         Nothing         -> return $ Vboard (fill (newBoard sz) sz defs values)
         Just (Vboard b) -> return $ Vboard (fill b sz defs values)
         _ -> error "TODO")
   where
      newBoard sz = array ((1,1), sz)
                    (zip [(x,y) | x <- [1..(fst sz)], y <- [1..(snd sz)]] (repeat (Vs "?")))
      fill board sz ds vs = foldl (\b p -> updateBoard b sz (fst p) (snd p)) board (zip ds vs)

updateBoard :: Board -> (Int, Int) -> (BoardEq a) -> Val -> Board
updateBoard b sz d v = let indices = range ((1,1), sz) in
                              b // zip (filter (posMatches (xpos d) (ypos d)) indices) (repeat v)

-- | Check if a Pos matches a coordinate pair
posMatches :: Pos -> Pos -> (Int, Int) -> Bool
posMatches xp yp (x, y) = match xp x && match yp y
   where
      match (ForAll _) _        = True
      match (Index ix) i = ix == i

-- | Attempt to access a position on the board that may be invalid
-- If the position is invalid, return an Err value instead of causing an actual array access error
tryUnsafeBoardAccess :: (Int,Int) -> Board -> Val
tryUnsafeBoardAccess (x,y) arr = let (_,(bx,by)) = bounds arr in
   case (x < 1 || x > bx || y < 1 || y > by) of
     False -> arr ! (x,y) -- good index
     True  -> Err $ p1 ++ p2
         where
            p1 = "Could not access (" ++ show x ++ "," ++ show y ++ ") on the board, " ++
              "this is not a valid space. "
            p2 = if bx == by && bx == 1 then "The board only has one space at (1,1)."
                                     else "The board size is ("++ show bx ++ "," ++ show by ++")."

-- | Binary operation evaluation
evalBinOp :: Op -> (Expr a) -> (Expr a) -> Eval Val
evalBinOp Plus l r     = evalNumOp "+" (+) l r
evalBinOp Minus l r    = evalNumOp "-" (-) l r
evalBinOp Times l r    = evalNumOp "*" (*) l r
evalBinOp Div l r      = evalNumOp "/" div l r
evalBinOp Mod l r      = evalNumOp "%" mod l r
evalBinOp Less l r     = evalCompareOpInt (<) l r
evalBinOp Leq l r      = evalCompareOpInt (<=) l r
evalBinOp NotEquiv l r = evalNotEquiv l r
evalBinOp Equiv l r    = evalEquiv l r
evalBinOp Geq l r      = evalCompareOpInt (>=) l r
evalBinOp Greater l r  = evalCompareOpInt (>) l r
evalBinOp Get l r      = do
   board <- eval l
   pos   <- eval r
   case (board, pos) of
      (Vboard arr, Vt [Vi x, Vi y]) -> return $ tryUnsafeBoardAccess (x,y) arr
      _ -> return $ Err $ "Could not access" ++ show l ++ " in " ++ show "r"

-- | evaluates the /= operation
evalNotEquiv l r = do
   eq <- evalEquiv l r
   case eq of
      (Vb b) -> return $ Vb (not b)
      eq     -> return eq

-- | evaluates the == operation
evalEquiv :: (Expr a) -> (Expr a) -> Eval Val
evalEquiv l r = do
   v1 <- eval l
   v2 <- eval r
   case (v1,v2) of
     (Vi l', Vi r') -> return $ Vb (l' == r')
     (Vs l', Vs r') -> return $ Vb (l' == r')
     (Vb l', Vb r')   -> return $ Vb (l' == r')
     (Vt l', Vt r')   -> return $ Vb (l' == r')
     (Vboard l', Vboard r') -> return $ Vb (l' == r')
     _              -> return $ Err $ "Could not compare " ++ (show l) ++ " to " ++ (show r)

-- | evaluates comparison operations for only Ints (except for == & /=)
evalCompareOpInt :: (Int -> Int -> Bool) -> (Expr a) -> (Expr a) -> Eval Val
evalCompareOpInt f l r = do
                     v1 <- eval l
                     v2 <- eval r
                     case (v1, v2) of
                        (Vi l', Vi r') -> return (Vb (f l' r'))
                        _ -> return $ Err $ "Could not compare " ++ (show l) ++ " to " ++ (show r)

-- | evaluates numerical operations
evalNumOp :: String -> (Int -> Int -> Int) -> (Expr a) -> (Expr a) -> Eval Val
evalNumOp sym f l r =
   do
        v1 <- eval l
        v2 <- eval r
        case (v1, v2) of
          -- if div/mod and denominator is 0, report an error, otherwise proceed
          (Vi l', Vi r') -> if r' == 0 && (sym == "/" || sym == "%")
                              then return (Err "Cannot divide by zero")
                              else return (Vi (f l' r'))
          _  -> return $
                  Err $ "Could not do numerical operation on " ++ (show l) ++ " to " ++ (show r)

eval :: (Expr a) -> Eval Val
eval (Annotation a e) = eval e
eval (I i) = return $ Vi i

eval (B b) = return $ Vb b

eval (S s) = return $ Vs s

eval (Tuple es) = mapM eval es >>= (return . Vt)

eval (Ref n) = do
  e <- lookupName n
  let b = lookup n builtinRefs
  case (e, b) of
        (Just v, _) -> case v of
          -- Pending Value, need to eval this to get the actual value
          (Pv evalenv e') -> evalWithLimit $ eval e'
          -- normal value, return as is
          v'              -> return $ v
        (_, Just v) -> v
        _ -> return $ Err $ "Variable " ++ n ++ " undefined"

eval (App n es) = do
  args <- eval es >>= \x -> case x of
    (Vt [Vt args]) -> return args
    (Vt args) -> return args
  f <- lookupName n
  case f of
    Just (Vf params env' e) -> extScope (zip params (args) ++ env') (evalWithLimit (eval e)) -- ++ env?
    Just (Pv env' e) -> extScope (zip [] (args) ++ env') (evalWithLimit (eval e)) -- ++ env?
    Nothing -> case lookup n builtins of
      Just f -> do
        (f (args))
      Nothing -> do
        return $ Err $ "Couldn't find " ++ n ++ " in environment!"
  where
    vals ((Vt xs):ys) = xs ++ (vals ys)
    vals (x:xs) = x : vals xs

eval (Let n e1 e2) = do
  v <- eval e1
  extScope (pure (n, v)) (eval e2)

eval (If p e1 e2) = do
  b <- unpackBool <$> (eval p)
  case b of
    (Just bb) -> if bb then evalWithLimit $ eval e1 else evalWithLimit $ eval e2
    Nothing   -> return $ Err e
      where e = "The expression " ++ show p ++ " did not evaluate to a Bool as expected!"

eval (Binop op e1 e2) = evalBinOp op e1 e2

eval (While c b names exprs) = do
   c' <- unpackBool <$> eval c   -- evaluate the condition
   case c' of
      (Just True)  -> do
         env <- getEnv         -- get the current environment
         result <- eval b      -- evaluate the body
         case result of        -- update the variables in the environment w/ new values and recurse:
            (Vt vs) -> extScope ((zip names vs) ++ env) recurse
            r       -> extScope ((head names, r) : env) recurse -- that head should never fail...
      (Just False) -> do
        e <- eval exprs
        return e
      Nothing      -> return $ Err $ "The expression " ++ show c ++
                                     " did not evaluate to a Bool as expected!"
   where
      recurse = evalWithLimit $ eval (While c b names exprs)

eval (HE n) = err ("Type hole: ")

runWithBuffer :: Env -> Buffer -> (Expr SourcePos) -> Either ([Val], Buffer) ([Val], Val)
runWithBuffer env buf e = do
  let v = runEval env buf (eval' e) in
    case v of
      Left (NeedInput bs) -> Left (bs, buf)
      Right (boards, val) -> Right (boards, val)
      Left (Error e)      -> Right $ ([], Err e) -- not good
      Left e              -> Right $ ([], Err ("Bad Error (Not Good): " ++ (show e))) -- not good
   where
      eval' :: (Expr a) -> Eval ([Val], Val)
      eval' expr = do
         v <- (eval expr)
         (_, boards,iters) <- get
         return (boards, v)
