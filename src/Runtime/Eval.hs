--- | Interpreter for Spiel

module Runtime.Eval where
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












type Board = Array (Int, Int) Val 

-- | Values
data Val = Vi Integer -- ^ Integer value
         | Vb Bool -- ^ Boolean value
         | Vpos (Int, Int) -- ^ Position value
         | Vboard Board -- ^ Board value (displayed to user)
         | Vt [Val] -- ^ Tuple value
         | Vs Name -- ^ Symbol value
         | Vf [Name] EvalEnv Expr -- ^ Function value
         | Err String -- ^ Runtime error (I think the typechecker catches all these)
         | Deferred -- ^ This needs an input.


-- | Can't compare two functions.
instance Eq Val where
  (Vi x) == (Vi y) = x == y
  (Vb b1) == (Vb b2) = b1 == b2
  (Vpos x) == (Vpos y) = x == y
  (Vboard b1) == (Vboard b2) = b1 == b2
  (Vt x) == (Vt y) = x == y
  (Vs n) == (Vs n2) = n == n2
  _ == _ = False

type EvalEnv = [(Name, Val)]

instance Show Val where
  show (Vi i) = show i
  show (Vb b) = show b
  show (Vpos x) = show x
  show (Vboard b) = "Board: " ++ show b
  show (Vt xs) = intercalate " " $ map show xs
  show (Vs s) = s
  show (Vf xs env' e) = "\\" ++ show xs ++ " -> " ++ show e
  show (Err s) = "ERR: " ++ s

-- | Call-by-value semantics
data Env = Env {
  evalEnv :: EvalEnv  ,
  boardSize :: (Int, Int)
               }
  deriving Show

emptyEnv x = Env [] x

modifyEval :: (EvalEnv -> EvalEnv) -> Env -> Env
modifyEval f (Env e b) = Env (f e) b

-- | Input buffer
type Buffer = [Val]
-- | Exceptions
data Exception =
  NeedInput Val | -- ^ Ran out of input, and here's the current board
  Error String -- ^ Encountered a runtime error (shouldn't ever happen)
  deriving Show

-- | Evaluation occurs in the Identity monad with these side effects:
-- ReaderT: Evaluation enviroment, board size and piece type, and input type
-- StateT: Input buffer, used for reading input
type Eval a = StateT Buffer (ExceptT Exception (ReaderT Env (Identity))) a

-- | Evaluate in the environment given, with a buffer.
runEval :: Env -> Buffer -> Eval a -> Either Exception a
runEval env buf x = runIdentity (runReaderT (runExceptT (evalStateT x buf)) env)

-- | Evaluate with an extended scope
extScope :: EvalEnv -> Eval a -> Eval a
extScope env = local (modifyEval (env++))

-- | Lookup a name in the environment
lookupName :: Name -> Eval (Maybe Val)
lookupName n = do
  env <- (evalEnv <$> ask)
  case lookup n env of
    Just v -> (return . Just) v
    Nothing -> return Nothing

-- | Ask for input, displaying a value to the user
waitForInput :: Val -> Eval a
waitForInput v = throwError (NeedInput v)

-- | Read input
readTape :: Eval (Val) -> Eval (Val)
readTape v = do
  tape <- get
  v' <- v
  case tape of
    (x:xs) -> (put xs) >> return x
    [] -> waitForInput v'

-- | Helper function to get the Bool out of a value. This is a partial function.
unpackBool :: Val -> Bool
unpackBool (Vb b) = b
unpackBool _ = undefined

-- | Produce all of the bindings from a list of value definitions. This is done sequentially.
bindings :: (Int, Int) -> [ValDef] -> Env
bindings sz vs = e
  where
    e = foldr (\(n, v) env -> case runEval env [] v of
                                Right v -> modifyEval ((n, v):) env
                                Left err -> env)
        (emptyEnv sz)
        (map bind vs)
   
-- | Bind the value of a definition to its name in the current Environment
bind :: ValDef -> (Name, Eval Val)
bind (Val _ (Veq n e)) = (n, eval e)
bind  (Val _ (Feq n (Pars ls) e)) = (n, do
                                        env <- evalEnv <$> ask
                                        return $ Vf ls env e)
bind (BVal _ (PosDef n xp yp e2)) = (n, do
      sz <- boardSize <$> ask
      value <- eval e2 
      maybeBoard <- lookupName n  
      case maybeBoard of 
         Nothing -> let board = array ((1,1), sz) (zip [(x,y) | x <- [1..(fst sz)], y <- [1..(snd sz)]] (repeat (Vs "?"))) in do  -- TODO: replace ? with a new Val? 
            return $ Vboard (updateBoard board sz xp yp value)
         Just (Vboard b) -> return $ Vboard (updateBoard b sz xp yp value)
         _ -> error "TODO")

updateBoard :: Board -> (Int, Int) -> Pos -> Pos -> Val -> Board 
updateBoard b sz xp yp v = let indices = range ((1,1), sz) in
                              b // zip (filter (posMatches xp yp) indices) (repeat v)    

-- | Check if a Pos matches a coordinate pair  
posMatches :: Pos -> Pos -> (Int, Int) -> Bool 
posMatches xp yp (x, y) = match xp x && match yp y 
   where  
      match ForAll _        = True 
      match (Index ix) i = ix == i 


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
evalBinOp Get l r   = do
                        board <- eval l 
                        pos   <- eval r 
                        case (board, pos) of   
                           (Vboard arr, Vpos (x,y)) -> return $ arr ! (x,y) 
                           _ -> return $ Err $ "Could not access" ++ show l ++ " in " ++ show "r" 
                           -- not a great error message, but this should be caught in the typechecker anyways  

        --[Vboard arr, Vpos (x,y)] -> return $ arr ! (x,y))
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

-- maybe [Eval Val] -> Eval Val, for symmetry with others?
builtins :: [(Name, [Expr] -> Eval Val)]
builtins = [
  ("input", \[v] -> readTape (eval v)),
  ("place", \xs -> do
      xs' <- mapM eval xs
      case xs' of
        [v, Vboard arr, Vpos (x,y)] -> return $ Vboard $ arr // [((x,y), v)]),
  ("remove", \xs -> do
      xs' <- mapM eval xs
      case xs' of
        [Vboard arr, Vpos (x,y)] -> return $ Vboard $ arr // pure ((x,y), Vs "Empty")),
  ("isFull", \xs -> do
      xs' <- mapM eval xs
      case xs' of
        [Vboard arr] -> return $ Vb $ all (/= Vs "Empty") $ elems arr),
  ("inARow", \xs -> do
      xs' <- mapM eval xs
      case xs' of
        [Vi i, v, Vboard arr] -> return $ Vb $ line v (assocs arr) (fromInteger i)),
  ("at", \xs -> do
      xs' <- mapM eval xs
      case xs' of
        [Vboard arr, Vpos (x,y)] -> return $ arr ! (x,y))
  ]

builtinRefs :: [(Name, Eval Val)]
builtinRefs = [("positions", (boardSize <$> ask) >>= \(szx, szy) -> return $ Vt [Vpos (x,y) | x <- [1..szx], y <- [1..szy]])]

eval :: Expr -> Eval Val
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
  f <- lookupName n
  case f of
    Just (Vf params env' e) -> extScope (zip params args ++ env') (eval e)
    Nothing -> case lookup n builtins of
      Just f -> do
        f es
      Nothing -> do
        return $ Err $ "Couldn't find " ++ n ++ " in environment!"

eval (Let n e1 e2) = do
  v <- eval e1
  extScope (pure (n, v)) (eval e2)

eval (If p e1 e2) = do
  b <- unpackBool <$> (eval p)
  if b then eval e1 else eval e2

eval (Binop op e1 e2) = evalBinOp op e1 e2

eval (Case n xs e)  = do
  f <- lookupName n
  case f of
    Just v -> case v of
      (Vs s) -> case lookup s (xs) of
        Just e' -> extScope (pure (n, v)) (eval e')
        Nothing -> extScope (pure (n, v)) (eval e) -- hmm.
      _ -> extScope (pure (n, v)) (eval e)
    Nothing -> undefined

eval (While c b names exprs) = do
   c' <- unpackBool <$> eval c   -- evaluate the condition 
   case c' of 
      True  -> do 
         env <- evalEnv <$> ask                                 -- get the current environment 
         result <- eval b                                       -- evaluate the body  
         case result of                                         -- update the variables in the environment w/ new values and recurse: 
            (Vt vs) -> extScope ((zip names vs) ++ env) recurse
            r       -> extScope ((head names, r) : env) recurse -- that head should never fail...famous last words
      False -> eval exprs 
   where 
      recurse = eval (While c b names exprs)    

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

runWithBuffer :: Env -> Buffer -> Expr -> Either (Val, Buffer) Val
runWithBuffer env buf e = do
  let v = runEval env buf (eval e) in
    case v of
      Left (NeedInput b) -> Left (b, buf)
      Right val -> Right val
      Left (Error e) -> error e -- not good

