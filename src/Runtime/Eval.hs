-- |

module Runtime.Eval where
import Language.Syntax
import Control.Monad
import Data.Array
import Control.Monad.Reader
import Control.Monad.Except
import Debug.Trace
type Env = [(Name, V)]

data V = Simple Expr Env | Fun [Name] Expr Env
  deriving Show

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
bindings vs = map (bind x) vs
  where
    x = map (bind []) vs

bind :: Env -> ValDef -> (Name, V)
bind env (Val _ (Veq n e)) = (n, Simple e env)
bind env (Val _ (Feq n (Pars ls) e)) = (n, Fun ls e env)
-- bval?
-- builtins

input :: [Expr] -> Eval Val
input [] = (Vpos . read) <$> (liftIO getLine)
input _ = undefined

builtins :: [(Name, [Expr] -> Eval Val)]
builtins = [("input", input)]

type Eval a = ReaderT Env (IO) a
-- | Evaluate an expression in the Eval Monad
eval :: Expr -> Eval Val
eval (I i) = return $ Vi i
eval (B b) = return $ Vb b
eval (S s) = return $ Vs s
eval (Tuple es) = (sequence (map eval es)) >>= (return . Vt)
eval (Ref n) = do
  e <- ask
  case lookup n e of
        Just (Simple v e') -> (trace $ "evaling " ++ show v) $ local (const $ e') (eval v)
        _ -> return $ Err $ "Variable " ++ n ++ " undefined"
eval (App n es) = do
  env <- ask
  args <- return $  map (\x -> Simple x env) es
  (trace $ show env) $ return ()
  case lookup n env of
    Just (Fun params e env') -> local (const $ ((zip params args)) ++ env') (eval e)
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
 
eval (Binop Plus e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (Vi x, Vi y) -> return $ Vi (x + y)
eval (Binop Less e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (Vi i1, Vi i2) -> return $ Vb (i1 < i2)


-- | evaluate an expression and run it
-- >>> run [] (I 2)
-- Result: Vi 2
-- >>> run [] (Tuple [I 2, I 3, I 4])
-- Result: Vt [Vi 2,Vi 3,Vi 4]
-- >>> run [] (Let "x" (I 2) (Ref "x"))
-- Result: Vi 2

run :: Env -> Expr -> IO ()
run env e = do
  v <- (runReaderT (eval e) env)
  putStrLn ("Result: " ++ show v)
enviroment = [("ten", (Fun ["x"] (Binop Less (Ref "x") (I 10)))), ("succ", (Fun ["x"] (Binop Plus (Ref "x") (I 1))))]
