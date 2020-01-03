-- |

module Runtime.Eval where
import Language.Syntax
import Control.Monad
import Data.Array

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

unpackBool :: Val -> Bool
unpackBool (Vb b) = b
unpackBool _ = undefined

bindings :: [ValDef] -> Env
bindings vs = map bind vs

bind :: ValDef -> (Name, V)
bind (Val _ (Veq n e)) = (n, Simple e)
bind (Val _ (Feq n (Pars ls) e)) = (n, Fun ls e)

-- builtins

input :: [Expr] -> IO Val
input [] = (Vpos . read) <$> getLine
input _ = undefined

builtins :: [(Name, [Expr] -> IO Val)]
builtins = [("input", input)]



eval :: Env -> Expr -> IO Val
eval e (I i) = return $ Vi i
eval e (B b) = return $ Vb b
eval e (S s) = return $ Vs s
eval e (Tuple es) = (sequence (map (eval e) es)) >>= (return . Vt)
eval e (Ref n) = case lookup n e of
  Just (Simple v) -> eval e v
  _ -> return $ Err $ "Variable " ++ n ++ " undefined"
eval env (App n es) = case lookup n env of
  Just (Fun params e) -> eval ((zip params args) ++ env) e
  Nothing -> case lookup n builtins of
    Just (f) -> f es
    Nothing -> return $ Err ""
  where
    args = map Simple es
eval env (Let n e1 e2) = eval ((n, Simple e1):env) e2
eval env (If p e1 e2) = do
  b <- unpackBool <$> (eval env p)
  if b then eval env e1 else eval env e2

-- eval env (While t p e) = do
