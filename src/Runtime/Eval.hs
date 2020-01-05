-- |

module Runtime.Eval where
import Language.Syntax
import Control.Monad
import Data.Array
import Control.Monad.Reader
import Control.Monad.Error

type Env = [(Name, V)]

data V = Simple Expr | Fun [Name] Expr

data Val = Vi Integer
         | Vb Bool
         | Vpos (Int, Int)
         | Vboard (Array Int Val)
         | Vt [Val]
         | Vs String
         | Vf [Name] Expr
         | Err String
         deriving Show

unpackBool :: Val -> Bool
unpackBool (Vb b) = b
unpackBool _ = undefined

bindings :: [ValDef] -> Env
bindings vs = map bind vs

bind :: ValDef -> (Name, V)
bind (Val _ (Veq n e)) = (n, Simple e)
bind (Val _ (Feq n (Pars ls) e)) = (n, Fun ls e)

-- builtins

input :: [Expr] -> Eval Val
input [] = (Vpos . read) <$> (liftIO getLine)
input _ = undefined

builtins :: [(Name, [Expr] -> Eval Val)]
builtins = [("input", input)]

type Eval a = ReaderT Env (IO) a

evalBinOp :: Op -> Expr -> Expr -> Eval Val 
evalBinOp Plus l r  = evalNumOp (+) l r  
evalBinOp Minus l r = evalNumOp (-) l r
evalBinOp Times l r = evalNumOp (*) l r
evalBinOp Div l r   = evalNumOp div l r 
evalBinOp Mod l r   = evalNumOp mod l r
evalBinOp Equiv l r = evalBoolOp (==) l r
evalBinOp Or l r    = evalBoolOp (&&) l r
evalBinOp And l r   = evalBoolOp (||) l r 
evalBinOp Xor l r   = evalBoolOp (/=) l r

-- |evaluates numerical operations 
evalNumOp :: (Integer -> Integer -> Integer) -> Expr -> Expr -> Eval Val 
evalNumOp f l r = do
                     v1 <- eval l 
                     v2 <- eval r 
                     case (v1, v2) of 
                        (Vi l', Vi r') -> return (Vi (f l' r'))
                        _ -> return $ Err $ "Could not do numerical operation on " ++ (show l) ++ " to " ++ (show r)  

-- |evaluates boolean operations 
evalBoolOp :: (Bool -> Bool -> Bool) -> Expr -> Expr -> Eval Val 
evalBoolOp f l r = do
                     v1 <- eval l 
                     v2 <- eval r 
                     case (v1, v2) of 
                        (Vb l', Vb r') -> return (Vb (f l' r'))
                        _ -> return $ Err $ "Could not do boolean operation on " ++ (show l) ++ " to " ++ (show r)  

-- | 
-- 
-- >>> evaluate [] (Binop Equiv (B True) (Binop And (B True) (B True)))
-- Result: Vb True 
-- 
-- >>> evaluate [] (Binop Plus (Binop Minus (I 1) (I 1)) (Binop Times (I 2) (I 3)))
-- Result: Vi 6  
-- 
-- >>> evaluate [] (Binop Plus (B True) (Binop Times (I 2) (I 3)))
-- Result: Err ...  
eval :: Expr -> Eval Val
eval (I i) = return $ Vi i
eval (B b) = return $ Vb b
eval (S s) = return $ Vs s
eval (Tuple es) = (sequence (map eval es)) >>= (return . Vt)
eval (Ref n) = do
  e <- ask
  case lookup n e of
        Just (Simple v) -> eval v
        _ -> return $ Err $ "Variable " ++ n ++ " undefined"
eval (App n es) = do
  env <- ask
  case lookup n env of
        Just (Fun params e) -> local ((++) (zip params args)) (eval e)
        Nothing -> case lookup n builtins of
                Just (f) -> f es
                Nothing -> return $ Err ""
  where
    args = map Simple es
eval (Let n e1 e2) = local ((:) (n, Simple e1)) (eval e2)
eval (If p e1 e2) = do
  b <- unpackBool <$> (eval p)
  if b then eval e1 else eval e2
eval (Binop o l r) = evalBinOp o l r 

-- eval env (While t p e) = do
evaluate :: Env -> Expr -> IO ()
evaluate env e = do
  v <- (runReaderT (eval e) env)
  putStrLn ("Result: " ++ show v)
