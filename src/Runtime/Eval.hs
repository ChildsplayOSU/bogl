-- |

module Runtime.Eval where
import Language.Syntax
import Control.Monad


type Env = [(Name, Val)]

data Val = Vi Integer
         | Vb Bool
         | Vt [Val]
         | Vf Env Name Expr


eval :: Env -> Expr -> Val
eval env (I i) = Vi i
eval env (B b) = Vb b
eval env (N n) = case lookup n env of
                   Just x -> x
                   _ -> undefined
eval env (Tuple xs) = Vt $ map (eval env) xs
eval env (Let n x x') = eval ((n, eval env x):env) x'
eval env (App f params) = case lookup f env of
                            Just x -> undefined
eval env (Binop x1 op x2) = case op of
                              _ -> undefined
                              -- etc
eval env (If p e1 e2) = case eval env p of
                          (Vb True) -> eval env e1
                          (Vb False) -> eval env e2
