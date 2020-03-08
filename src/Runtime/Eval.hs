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

import Text.Parsec.Pos


import Debug.Trace






-- | Produce all of the bindings from a list of value definitions. This is done sequentially. Errors are reported as they're found.
bindings :: (Int, Int) -> [ValDef a] -> Writer [Exception] Env
bindings sz vs = e
  where
    e = foldM (\env (n, v) -> case runEval env [] v of
                                Right v' -> return $ modifyEval ((n, v'):) env
                                Left err -> (tell [err]) >> return env)
        (emptyEnv sz)
        (reverse $ map bind vs)

bindings_ x y = (fst . runWriter) (bindings x y)

bind :: (ValDef a) -> (Name, Eval Val)
bind (Val _ (Veq n e) _) = (n, eval e)
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
      newBoard sz = array ((1,1), sz) (zip [(x,y) | x <- [1..(fst sz)], y <- [1..(snd sz)]] (repeat (Vs "?"))) -- TODO: replace ? 
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


-- | Binary operation evaluation
evalBinOp :: Op -> (Expr a) -> (Expr a) -> Eval Val
evalBinOp Plus l r  = evalNumOp (+) l r
evalBinOp Minus l r = evalNumOp (-) l r
evalBinOp Times l r = evalNumOp (*) l r
evalBinOp Div l r   = evalNumOp div l r
evalBinOp Mod l r   = evalNumOp mod l r
evalBinOp Equiv l r = evalEquiv l r
evalBinOp Or l r    = evalBoolOp (||) l r
evalBinOp Less l r  = evalCompareOp (<) l r
evalBinOp And l r   = evalBoolOp (&&) l r
evalBinOp Xor l r   = evalBoolOp (/=) l r
evalBinOp Get l r   = do
                        board <- eval l
                        pos   <- eval r
                        case (board, pos) of
                           (Vboard arr, Vpos (x,y)) -> return $ arr ! (x,y)
                           _ -> return $ Err $ "Could not access" ++ show l ++ " in " ++ show "r"
                           -- not a great error message, but this should be caught in the typechecker anyways

        --[Vboard arr, Vpos (x,y)] -> return $ arr ! (x,y))
-- | evaluates the == operation
evalEquiv :: (Expr a) -> (Expr a) -> Eval Val
evalEquiv l r = do
                  v1 <- eval l
                  v2 <- eval r
                  return $ Vb (v1 == v2)

-- | evaluates comparison operations (except for ==)
evalCompareOp :: (Int -> Int -> Bool) -> (Expr a) -> (Expr a) -> Eval Val
evalCompareOp f l r = do
                     v1 <- eval l
                     v2 <- eval r
                     case (v1, v2) of
                        (Vi l', Vi r') -> return (Vb (f l' r'))
                        _ -> return $ Err $ "Could not compare " ++ (show l) ++ " to " ++ (show r)

-- | evaluates numerical operations
evalNumOp :: (Int -> Int -> Int) -> (Expr a) -> (Expr a) -> Eval Val
evalNumOp f l r = do
                     v1 <- eval l
                     v2 <- eval r
                     case (v1, v2) of
                        (Vi l', Vi r') -> return (Vi (f l' r'))
                        _ -> return $ Err $ "Could not do numerical operation on " ++ (show l) ++ " to " ++ (show r)

-- | evaluates boolean operations
evalBoolOp :: (Bool -> Bool -> Bool) -> (Expr a) -> (Expr a) -> Eval Val
evalBoolOp f l r = do
                     v1 <- eval l
                     v2 <- eval r
                     case (v1, v2) of
                        (Vb l', Vb r') -> return (Vb (f l' r'))
                        _ -> return $ Err $ "Could not do boolean operation on " ++ (show l) ++ " to " ++ (show r)


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
        (Just v, _) -> return $ v
        (_, Just v) -> v
        _ -> return $ Err $ "Variable " ++ n ++ " undefined"

eval (App n es) = do
  args <- mapM eval es
  let args' = foldr (\x k -> case x of
        (Vt xs) -> xs ++ k
        x -> x:k) [] args -- NOT GOOD
  f <- lookupName n
  case f of
    Just (Vf params env' e) -> extScope (zip params (args') ++ env') (eval e) -- ++ env?
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
  if b then eval e1 else eval e2

eval (Binop op e1 e2) = evalBinOp op e1 e2

eval (While c b names exprs) = do
   c' <- unpackBool <$> eval c   -- evaluate the condition
   case c' of
      True  -> do
         env <- getEnv                          -- get the current environment
         result <- eval b                                       -- evaluate the body
         case result of                                         -- update the variables in the environment w/ new values and recurse:
            (Vt vs) -> extScope ((zip names vs) ++ env) recurse
            r       -> extScope ((head names, r) : env) recurse -- that head should never fail...famous last words
      False -> do
        e <- eval exprs
        return e
   where
      recurse = eval (While c b names exprs)

eval (HE n) = err ("Type hole: ")
-- | Run an 'Expr' in the given 'Env' and display the result
--
-- >>> run [] (I 2)
-- 2
--
-- >>> run [] (Tuple [I 2, I 3, I 4])
-- 2 3 4
--
-- >>> run [] (Let "x" (I 2) (Ref "x"))
-- 2

runWithBuffer :: Env -> Buffer -> (Expr SourcePos) -> Either (Val, Buffer) Val
runWithBuffer env buf e = do
  let v = runEval env buf (eval e) in
    case v of
      Left (NeedInput b) -> Left (b, buf)
      Right val -> Right val
      Left (Error e) -> Right $ Err e -- not good
