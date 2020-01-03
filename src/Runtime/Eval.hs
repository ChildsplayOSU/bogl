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

-- eval env (While t p e) = do
evaluate :: Env -> Expr -> IO ()
evaluate env e = do
  v <- (runReaderT (eval e) env)
  putStrLn ("Result: " ++ show v)
