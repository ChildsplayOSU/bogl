-- | Interpreter for Spiel

module Runtime.Eval (run, bindings) where
import Language.Syntax
import Control.Monad
import Data.Array
import Data.List
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity
import Debug.Trace

-- | Call-by-value semantics
type Env = [(Name, Val)]

-- | Evaluation occurs in the IO monad with an enviroment to read from.
-- alternate definition:
-- @type Eval a = ReaderT (Env, InputTape) (Identity) a@
-- for the 'pure' evaluator
type Eval a = ReaderT Env (Identity) a

newScope :: Env -> Eval a -> Eval a
newScope env = local (env++)

lookupName :: Name -> Eval (Maybe Val)
lookupName n = ask >>= (return . (lookup n))



-- | Values
data Val = Vi Integer -- ^ Integer value
         | Vb Bool -- ^ Boolean value
         | Vpos (Int, Int) -- ^ Position value
         | Vboard (Array Int Val) -- ^ Board value (displayed to user)
         | Vt [Val] -- ^ Tuple value
         | Vs Name -- ^ Symbol value
         | Vf [Name] Env Expr -- ^ Function value
         | Err String -- ^ Runtime error (I think the typechecker catches all these)
         deriving (Eq)

instance Show Val where
  show (Vi i) = show i
  show (Vb b) = show b
  show (Vpos x) = show x
  show (Vboard _) = "Board"
  show (Vt xs) = intercalate " " $ map show xs
  show (Vs s) = s
  show (Vf xs _ e) = "\\" ++ show xs ++ " -> " ++ show e
  show (Err s) = "ERR: " ++ s

-- | Helper function to get the Bool out of a value.
unpackBool :: Val -> Bool
unpackBool (Vb b) = b
unpackBool _ = undefined

-- | Produce all of the bindings from a list
-- Note that this is a little bizarre: x is the list of all bindings in an empty enviroment,
-- which is then used as the enviroment in a second pass.
bindings :: [ValDef] -> Env
bindings vs = map (bind x) vs
  where
    x = map (bind []) vs

-- | Bind the value of a definition to its name in the current Enviroment
bind :: Env -> ValDef -> (Name, Val)
bind env (Val _ (Veq n e)) = (n, v)
  where
    v = runIdentity (runReaderT (eval e) env)
bind env (Val _ (Feq n (Pars ls) e)) = (n, Vf ls env e)

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
eval :: Expr -> Eval Val
eval (I i) = return $ Vi i
eval (B b) = return $ Vb b
eval (S s) = return $ Vs s
eval (Tuple es) = (sequence (map eval es)) >>= (return . Vt)
eval (Ref n) = do
  e <- lookupName n
  case e of
        Just (v) -> return $ v
        _ -> return $ Err $ "Variable " ++ n ++ " undefined"
eval (App n es) = do
  args <- sequence $ map (eval) es
  f <- lookupName n
  case f of
    Just (Vf params env' e) -> newScope ((zip params args) ++ env') (eval e)
    Nothing -> undefined -- check if its a builtin? TODO
eval (Let n e1 e2) = do
  v <- eval e1
  newScope (pure (n, v)) (eval e2)
 
eval (If p e1 e2) = do
  b <- unpackBool <$> (eval p)
  if b then eval e1 else eval e2

eval (While p f x) = do
  b <- eval (App p [x])
  case b of
    (Vb b) -> if b then eval (While p f (App f [x])) else eval x
    _ -> undefined
   
eval (Binop op e1 e2) = evalBinOp op e1 e2

eval (Case n xs e)  = do
  f <- lookupName n
  case f of
    Just v -> case v of
      (Vs s) -> case lookup s xs of
        Just e' -> newScope (pure (n, v)) (eval e')
        _ -> undefined
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
run :: Env -> Expr -> IO ()
run env e = let v = runIdentity (runReaderT (eval e) env) in
  (putStrLn . show) v

