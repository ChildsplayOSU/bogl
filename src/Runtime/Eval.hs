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
         | ValD Expr


-- Can't compare two function

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

modifyEval :: (EvalEnv -> EvalEnv) -> Env -> Env
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
lookupName n = do
  env <- (evalEnv <$> ask)
  case lookup n env of
    Just v -> (return . Just) v
    Nothing -> return Nothing


waitForInput :: Val -> Eval a
waitForInput v = throwError (NeedInput v)

readTape :: Eval (Val) -> Eval (Val)
readTape v = do
  tape <- get
  v' <- v
  case tape of
    (x:xs) -> (put xs) >> return x
    [] -> waitForInput v'


-- | Helper function to get the Bool out of a value.
unpackBool :: Val -> Bool
unpackBool (Vb b) = b
unpackBool _ = undefined

--- | Produce all of the bindings from a list
bindings :: (Int, Int) -> [ValDef] -> Eval Env
bindings sz vs = do
  vs' <- mapM bind vs
  return $ Env (vs') sz

-- | Bind the value of a definition to its name in the current Environment
-- asking for input has to be deferred...
-- could have a seperate, declaration environment.
bind :: ValDef -> Eval (Name, Val)
bind (Val _ (Veq n e)) = do
  return (n, ValD e)
-- bind env (BVal _ (PosDef n e1 e2 e)) = (n, v) -- wrong!
--    where
--     (v1, v2) = (fromRight Deferred (runWithTape env [] e1), fromRight Deferred (runWithTape env [] e2))
--     v = fromRight Deferred (runWithTape env [] (e))
bind  (Val _ (Feq n (Pars ls) e)) = do
  env <- evalEnv <$> ask
  return (n, Vf ls env e)

-- look up the board and update it if it exists 
bind (BVal _ (PosDef n xp yp e2)) = do
      sz <- boardSize <$> ask
      value <- eval e2 
      maybeBoard <- lookupName n  
      case maybeBoard of 
         Nothing -> let board = array ((1,1), sz) (zip [(x,y) | x <- [1..(fst sz)], y <- [1..(snd sz)]] (repeat (Vs "?"))) in do  -- TODO: replace ? with a new Val? 
            return (n, Vboard (updateBoard board sz xp yp value)) 
         Just (Vboard b) -> return $ (n, Vboard (updateBoard b sz xp yp value)) 
         _ -> error "TODO"  

--bind (BVal _ (RegDef n e1 e2)) = do
--      sz <- boardSize <$> ask
--      v <- ((map dePos) . deTuple) <$> eval e1
--      value <- (eval e2)
--      return $ (n, Vboard $ array ((1,1), sz) (zip v (repeat value)))
--    where
--    dePos (Vpos (x,y)) = (x,y)
--    deTuple (Vt xs) = xs -- hmm

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
        [Vi i, Vboard arr, v] -> return $ Vb $ line v (assocs arr) (fromInteger i)),
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
eval (Tuple es) = (sequence (map eval es)) >>= (return . Vt)
eval (Ref n) = do
  traceM ("looking up variable " ++ n) 
  e <- lookupName n
  case e of
        Just (ValD e) -> eval e
        Just (v) -> return $ v
        Nothing -> case lookup n builtinRefs of
          Just v -> v
          Nothing -> return $ Err $ "Variable " ++ n ++ " undefined"
eval (App n es) = do
  args <- mapM eval es
  f <- lookupName n
  traceM "APP !"  
  case f of
    Just (Vf params env' e) -> newScope (zip params args ++ env') (eval e)
    Nothing -> case lookup n builtins of
      Just f -> do
        f es
      Nothing -> do
        env <- ask
        traceM (show env)
        traceM ("couldn't find " ++ n)
        return $ Err $ "Couldn't find" ++ n
eval (Let n e1 e2) = do
  v <- eval e1
  newScope (pure (n, v)) (eval e2)
eval (If p e1 e2) = do
  b <- unpackBool <$> (eval p)
  if b then eval e1 else eval e2

eval (Binop op e1 e2) = evalBinOp op e1 e2
eval (Case n xs e)  = do
  f <- lookupName n
  case f of
    Just v -> case v of
      (Vs s) -> case lookup s (xs) of
        Just e' -> newScope (pure (n, v)) (eval e')
        Nothing -> newScope (pure (n, v)) (eval e) -- hmm.
      _ -> newScope (pure (n, v)) (eval e)
    Nothing -> undefined
eval (While c b names exprs) = do
   c' <- unpackBool <$> eval c   -- evaluate the condition 
   case c' of 
      True  -> do 
         env <- evalEnv <$> ask                                 -- get the current environment 
         result <- eval b                                       -- evaluate the body  
         case result of                                         -- update the variables in the environment w/ new values and recurse: 
            (Vt vs) -> newScope ((zip names vs) ++ env) recurse 
            r       -> newScope ((head names, r) : env) recurse -- that head should never fail...famous last words 
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
emptyEnv = Env [] (3,3)

produceEnv :: Eval Env -> Either Exception Env
produceEnv e = runIdentity (runReaderT (runExceptT (evalStateT e [])) emptyEnv)

runWithTape :: Eval Env -> Tape -> Expr -> Either (Val, Tape) Val
runWithTape env tape e = do
  let Right env' = produceEnv env
  let v = runEval (eval e) env' tape in
    case v of
      Left (NeedInput b) -> Left (b, tape)
      Right val -> Right val
      _ -> undefined

runUntilComplete :: Eval Env -> Expr -> IO ()
runUntilComplete env expr = runUntilComplete' []
  where
    runUntilComplete' tape = case runWithTape env tape expr of
      Right v -> (putStrLn . show) v
      Left (b, t) -> do
        -- work out which type of input we want..
        (putStrLn . show) t
        (putStrLn . show) b
        x <- read <$> getLine
        y <- read <$> getLine
        runUntilComplete' (tape ++ pure (Vpos (x, y)))

