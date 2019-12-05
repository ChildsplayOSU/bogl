-- |

module Runtime.Eval where
import Language.Syntax
import Control.Monad


type Env = [(Name, Val)]

data Val = Vi Integer
         | Vb Bool
         | Vt [Val]
         | Vf Env Name Expr
{-
eval :: Monad m => (Val -> m ()) -> Env -> Expr -> m Val
eval o env (I i) = return $ Vi i
eval o env (B b) = return $ Vb b
eval o env (N n) = case lookup n env of
                   Just x -> return $ x
                   _ -> undefined
eval o env (Tuple xs) = (sequence $ (map (eval env) xs)) >>= (return . Vt)

eval o env (Let n x x') = do
  v <- eval env x
  eval ((n, v):env) x'
 
eval o env (App f params) = return $ case lookup f env of
                            Just x -> undefined

eval o env (Binop x1 op x2) = return $ case op of
                              _ -> undefined
                              -- etc
eval o env (If p e1 e2) = eval env p >>= \x -> case x of
                          (Vb True) -> eval env e1
                          (Vb False) -> eval env e2
                         
                          -- Semantics of while is confusing...
eval o env (While p e) = eval env p >>= \x -> case x of
                         (Vb True) -> eval env e >> (eval env (While p e))
                         (Vb False) -> eval env e -- this is definitely wrong.
-}
