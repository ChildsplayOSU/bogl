-- | Interpreter for Spiel

module Runtime.Eval (runWithTape, runUntilComplete, bindings) where
import Language.Syntax
import Control.Monad
import Data.Array
import Data.List

import Data.Either

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Debug.Trace

-- | Call-by-value semantics
data Env = Env {
  evalEnv :: EvalEnv  ,
  boardSize :: (Int, Int)
               }
           deriving (Eq, Show)

modifyEval :: ([(Name, Val)] -> [(Name, Val)]) -> Env -> Env
modifyEval f (Env e b) = Env (f e) b
-- | Input tape
type Tape = [Val]
-- | Exceptions
data Exception =
  NeedInput Val | -- ^ Ran out of input, and here's the current board
  Error String -- ^ Encountered a runtime error (shouldn't ever happen)
  deriving Show
-- | Evaluation occurs in the Identity monad with these side effects:
-- ReaderT: Evaluation enviroment, board size and piece type, and input type
-- StateT: Input tape
-- alternate definition:
-- @type Eval a = ReaderT (Env, InputTape) (Identity) a@
-- for the 'pure' evaluator
type Eval a = StateT Tape (ExceptT Exception (ReaderT Env (Identity))) a

type PreEval a = ReaderT Env (Identity)

runEval :: Eval a -> Env -> Tape -> Either Exception a
runEval x env tape  = runIdentity (runReaderT (runExceptT (evalStateT x tape)) env)

newScope :: EvalEnv -> Eval a -> Eval a
newScope env = local (modifyEval (env++))

lookupName :: Name -> Eval (Maybe Val)
lookupName n = (evalEnv <$> ask) >>= (return . (lookup n))

waitForInput :: Val -> Eval a
waitForInput b = throwError $ NeedInput b

readTape :: Val -> Eval (Val)
readTape v = do
  tape <- get
  case tape of
    (x:xs) -> (put xs) >> return x
    [] -> waitForInput v


-- | Helper function to get the Bool out of a value.
unpackBool :: Val -> Bool
unpackBool (Vb b) = b
unpackBool _ = undefined

-- | Produce all of the bindings from a list
-- Note that this is a little bizarre: x is the list of all bindings in an empty enviroment,
-- which is then used as the enviroment in a second pass.
bindings :: (Int, Int) -> [ValDef] -> Env
bindings sz vs = Env (map (bind (Env x sz)) vs) sz
  where
    x = map (bind (Env [] sz)) vs

-- | Bind the value of a definition to its name in the current Enviroment
-- asking for input has to be deferred...
bind :: Env -> ValDef -> (Name, Val)
bind env (Val _ (Veq n e)) = (n, v)
  where
    v = fromRight Deferred (runWithTape env [] (e))
-- bind env (BVal _ (PosDef n e1 e2 e)) = (n, v) -- wrong!
--    where
--     (v1, v2) = (fromRight Deferred (runWithTape env [] e1), fromRight Deferred (runWithTape env [] e2))
--     v = fromRight Deferred (runWithTape env [] (e))
bind (Env env _) (Val _ (Feq n (Pars ls) e)) = (n, Vf ls env e)
bind env@(Env _ sz) (BVal _ (RegDef n e1 e2)) = (n, v)
  where
    v = Vboard $ array ((1,1), sz) (zip ps (repeat value))
    ps = map dePos $ deTuple $ (fromRight Deferred (runWithTape env [] e1))
    value = fromRight Deferred (runWithTape env [] e2)
    dePos (Vpos (x,y)) = (x,y)
    deTuple (Vt xs) = xs -- hmm


-- | Binary operation evaluation
evalBinOp :: Op -> Expr -> Expr -> Eval Val 
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

-- | evaluates the == operation  
evalEquiv :: Expr -> Expr -> Eval Val 
evalEquiv l r = do
                  v1 <- eval l 
                  v2 <- eval r
                  return $ Vb (v1 == v2)

-- | evaluates comparison operations (except for ==) 
evalCompareOp :: (Integer -> Integer -> Bool) -> Expr -> Expr -> Eval Val 
evalCompareOp f l r = do
                     v1 <- eval l 
                     v2 <- eval r 
                     case (v1, v2) of 
                        (Vi l', Vi r') -> return (Vb (f l' r'))
                        _ -> return $ Err $ "Could not compare " ++ (show l) ++ " to " ++ (show r)  

-- | evaluates numerical operations 
evalNumOp :: (Integer -> Integer -> Integer) -> Expr -> Expr -> Eval Val 
evalNumOp f l r = do
                     v1 <- eval l 
                     v2 <- eval r 
                     case (v1, v2) of 
                        (Vi l', Vi r') -> return (Vi (f l' r'))
                        _ -> return $ Err $ "Could not do numerical operation on " ++ (show l) ++ " to " ++ (show r)  

-- | evaluates boolean operations 
evalBoolOp :: (Bool -> Bool -> Bool) -> Expr -> Expr -> Eval Val 
evalBoolOp f l r = do
                     v1 <- eval l 
                     v2 <- eval r 
                     case (v1, v2) of 
                        (Vb l', Vb r') -> return (Vb (f l' r'))
                        _ -> return $ Err $ "Could not do boolean operation on " ++ (show l) ++ " to " ++ (show r)  

-- | Evaluate an expression in the Eval Monad
-- 
-- >>> run [] (Binop Equiv (B False) (Binop And (B True) (B False)))
-- True 
-- 
-- >>> run [] (Binop Equiv (I 3) (I 4))
-- False 
--
-- >>> run [] (Binop Less (I 3) (I 4))
-- True 
--
-- >>> run [] (Binop Plus (Binop Minus (I 1) (I 1)) (Binop Times (I 2) (I 3)))
-- 6  
-- 
-- >>> run [] (Binop Plus (B True) (Binop Times (I 2) (I 3)))
-- ERR: ...

-- 🤔... this certainly isn't winning any prizes for efficiency.
inARow :: Val -> [((Int, Int), Val)] -> [((Int, Int), Val)] -> (Int, Int) -> Int -> Bool
inARow v state (s:st) d n = (inARow' state s d n) || inARow v state st d n
  where
    inARow' :: [((Int, Int), Val)] -> ((Int, Int), Val) -> (Int, Int) -> Int -> Bool
    inARow' _ _ _ 1 = True
    inARow' st ((x, y), c) (dx, dy) n = if v /= c then False else case lookup (x+dx, y+dy) st of
      Nothing -> False
      Just c' -> if v == c' then inARow' state ((x+dx, y+dy), c) (dx, dy) (n-1) else False
inARow _ _ _ _ _ = False

line :: Val -> [((Int, Int), Val)] -> Int -> Bool
line v acc n = (inARow v acc acc (1,1) n) ||  (inARow v acc acc (0,1) n) ||  (inARow v acc acc (1,0) n)



builtins :: [(Name,[Val] -> Eval Val)]
builtins = [
  ("input", \[v] -> readTape v),
  ("place", \[piece, Vboard arr, Vpos (x,y)] -> return $ Vboard $ arr // [((x,y), piece)]),
  ("remove", \[Vboard arr, Vpos (x,y)] -> return $ Vboard $ arr // pure ((x,y), Vs "Empty")),
  ("isFull", \[Vboard arr] -> return $ Vb $ all (/= Vs "Empty") $ elems arr),
  ("inARow", \[Vi i, Vboard arr, v] -> return $ Vb $ line v (assocs arr) (fromInteger i)),
  ("at", \[Vboard arr, Vpos (x,y)] -> return $ arr ! (x,y))
  ]

builtinRefs :: [(Name, Eval Val)]
builtinRefs = [("positions", (boardSize <$> ask) >>= \(szx, szy) -> return $ Vt [Vpos (x,y) | x <- [1..szx], y <- [1..szy]])]

eval :: Expr -> Eval Val
eval (I i) = return $ Vi i
eval (B b) = return $ Vb b
eval (S s) = return $ Vs s
eval (Tuple es) = (sequence (map eval es)) >>= (return . Vt)
eval (Ref n) = do
  e <- lookupName n
  case e of
        Just (v) -> return $ v
        Nothing -> case lookup n builtinRefs of
          Just v -> v
          Nothing -> return $ Err $ "Variable " ++ n ++ " undefined"
eval (App n es) = do
  args <- sequence $ map (eval) es
  f <- lookupName n
  case f of
    Just (Vf params env' e) -> newScope ((zip params args) ++ env') (eval e)
    Nothing -> case lookup n builtins of
      Just f -> f args
      Nothing -> undefined
eval (Let n e1 e2) = do
  v <- eval e1
  newScope (pure (n, v)) (eval e2)
eval (If p e1 e2) = do
  b <- unpackBool <$> (eval p)
  if b then eval e1 else eval e2

--- While loops are weird.
eval (While p f x) = do
  b <- eval (App p [x])
  case b of
    (Vb b) -> if b then (force $ (App f [x])) >>= (\e ->eval (While p f e)) else eval x
    _ -> undefined
  where
    force :: Expr -> Eval Expr
    force e = Value <$> eval e -- locks the evaluation in here, so it doesn't cost any side effects to evaluate again. kind of like program rewriting?

eval (Value v)    = return v
eval (Binop op e1 e2) = evalBinOp op e1 e2

eval (Case n xs e)  = do
  f <- lookupName n
  case f of
    Just v -> case v of
      (Vs s) -> case lookup s xs of
        Just e' -> newScope (pure (n, v)) (eval e')
        Nothing -> (traceM $ "Strange: " ++ show v ++ show s ++ show xs) >> return undefined
      _ -> newScope (pure (n, v)) (eval e)
    Nothing -> undefined

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
runWithTape :: Env -> Tape -> Expr -> Either Exception Val
runWithTape env tape e = do
  let v = runEval (eval e) env tape in v

runUntilComplete :: Env -> Expr -> IO ()
runUntilComplete env expr = runUntilComplete' []
  where
    runUntilComplete' tape = case runWithTape env tape expr of
      Right v -> (putStrLn . show) v
      Left (NeedInput b) -> do
        -- work out which type of input we want..
        (putStrLn . show) tape
        (putStrLn . show) b
        x <- read <$> getLine
        y <- read <$> getLine
        runUntilComplete' (tape ++ pure (Vpos (x, y)))
