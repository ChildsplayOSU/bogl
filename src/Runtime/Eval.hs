-- | Interpreter for Spiel

module Runtime.Eval (run, bindings) where
import Language.Syntax
import Control.Monad
import Data.Array
import Control.Monad.Reader
import Control.Monad.Except
import Debug.Trace

-- | Call-by-name semantics
type Env = [(Name, V)]

-- | Evaluation occurs in the IO monad with an enviroment to read from.
-- alternate definition:
-- @type Eval a = ReaderT (Env, InputTape) (Identity) a@
-- for the 'pure' evaluator
type Eval a = ReaderT Env (IO) a

-- | Either a simple expression (not a function) or a function. FIXME: this can be collapsed into a single type if the App rule and syntax is changed
data V = Simple Expr Env | Fun [Name] Expr Env
  deriving Show

-- | Values
data Val = Vi Integer -- ^ Integer value
         | Vb Bool -- ^ Boolean value
         | Vpos (Int, Int) -- ^ Position value
         | Vboard (Array Int Val) -- ^ Board value (displayed to user)
         | Vt [Val] -- ^ Tuple value
         | Vs Name -- ^ Symbol value
         | Vf [Name] Expr -- ^ Function value
         | Err String -- ^ Runtime error (I think the typechecker catches all these)
         deriving (Show, Eq)


-- | Helper function to get the Bool out of a value.
unpackBool :: Val -> Bool
unpackBool (Vb b) = b
unpackBool _ = undefined

-- | Produce all of the bindings from a list
bindings :: [ValDef] -> Env
bindings vs = map (bind x) vs
  where
    x = map (bind []) vs

-- | Bind an individual valdef to its name in the current Enviroment
bind :: Env -> ValDef -> (Name, V)
bind env (Val _ (Veq n e)) = (n, Simple e env)
bind env (Val _ (Feq n (Pars ls) e)) = (n, Fun ls e env)
-- bval?
-- builtins

-- | Builtin input function (SUBJECT TO CHANGE)
input :: [Expr] -> Eval Val
input [] = (Vpos . read) <$> (liftIO getLine)
input _ = undefined

-- | List of all the builtins in the builtin enviroment
builtins :: [(Name, [Expr] -> Eval Val)]
builtins = [("input", input)]

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
-- Result: Vb True 
-- 
-- >>> run [] (Binop Equiv (I 3) (I 4))
-- Result: Vb False 
--
-- >>> run [] (Binop Less (I 3) (I 4))
-- Result: Vb True 
--
-- >>> run [] (Binop Plus (Binop Minus (I 1) (I 1)) (Binop Times (I 2) (I 3)))
-- Result: Vi 6  
-- 
-- >>> run [] (Binop Plus (B True) (Binop Times (I 2) (I 3)))
-- Result: Err ...  
eval :: Expr -> Eval Val
eval (I i) = return $ Vi i
eval (B b) = return $ Vb b
eval (S s) = return $ Vs s
eval (Tuple es) = (sequence (map eval es)) >>= (return . Vt)
eval (Ref n) = do
  e <- ask
  case lookup n e of
        Just (Simple v e') -> local (const $ e') (eval v)
        _ -> return $ Err $ "Variable " ++ n ++ " undefined"
eval (App n es) = do
  env <- ask
  args <- return $  map (\x -> Simple x env) es

  case lookup n env of
    Just (Fun params e env') -> local (const $ ((zip params args)) ++ env' ++ env) (eval e) -- FIXME
    Nothing -> case lookup n builtins of
        Just (f) -> f es
        Nothing -> return $ Err $ "Couldn't find " ++ n ++ "in enviroment!"
eval (Let n e1 e2) = do
  env <- ask
  local ((:) (n, Simple e1 env)) (eval e2)
eval (If p e1 e2) = do
  b <- unpackBool <$> (eval p)
  if b then eval e1 else eval e2
eval (While p f x) = do
  b <- eval (App p [x])
  case b of
    (Vb b) -> if b then eval (While p f (App f [x])) else eval x
    _ -> undefined
eval (Binop op e1 e2) = evalBinOp op e1 e2

 


-- | evaluate an expression and run it
-- >>> run [] (I 2)
-- Result: Vi 2
-- >>> run [] (Tuple [I 2, I 3, I 4])
-- Result: Vt [Vi 2,Vi 3,Vi 4]
-- >>> run [] (Let "x" (I 2) (Ref "x"))
-- Result: Vi 2
-- | Run an 'Expr' in the given 'Env' and display the result
run :: Env -> Expr -> IO ()
run env e = (runReaderT (eval e) env) >>= (putStrLn . show)

-- | an example enviroment
enviroment = [("ten", (Fun ["x"] (Binop Less (Ref "x") (I 10)))), ("succ", (Fun ["x"] (Binop Plus (Ref "x") (I 1))))]
