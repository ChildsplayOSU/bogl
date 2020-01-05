-- |

module Runtime.Typechecker where

import Language.Syntax
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.Except
import Debug.Trace
import Data.Either
type Error = String
type Env = [(String, Type)]

data TypeError = WrongType String | NotBound String
  deriving Show

type Typechecked a =  (ReaderT Env (ExceptT TypeError Identity)) a


detuple :: Ptype -> [Type]
detuple (Pt (Tup ts)) = map Plain (map Pext ts)
detuple x = (trace $ show x) $ undefined

deftype :: ValDef -> Typechecked Type
deftype (Val (Sig n t) eqn) = do
  eqt <- eqntype t eqn
  if eqt == t then return t else undefined -- exceptT


eqntype :: Type -> Equation -> Typechecked Type
eqntype _ (Veq _ e) = exprtype e >>= (return . Plain)
eqntype (Function (Ft inputs _)) (Feq _ (Pars params) e) = do
    i <- return $ case inputs of
                t@(Pt (Tup ts)) -> detuple t
                x -> [Plain x]
    e' <- local ((++) (zip params (i))) (exprtype e)
    return $ Function (Ft inputs (e'))
eqntype _ _ = throwError (WrongType "ERR!")



exprtype :: Expr -> Typechecked Ptype
exprtype (I _) = return $ (Pext (X Itype []))
exprtype (S _) = return $ (Pext (X Symbol []))
exprtype (B _) = return $ (Pext (X Booltype []))
exprtype (Ref s) = do
  x <- getType s
  case x of
    (Plain t) -> return t
    other -> throwError (WrongType $ "Couldn't defererence type " ++ show other)  -- exceptT
exprtype (Tuple xs) = do
  xs' <- (sequence $ map exprtype xs)
  return $ (Pt (Tup (map extract xs')))
exprtype (App n es) = do
  es' <- sequence $ map exprtype es
  t <- getType n
  case t of
        (Function (Ft (Pt (Tup i)) o)) -> if map extract es' == i then return o else (throwError (WrongType $ "Couldn't match type " ++ show es' ++ " with type " ++ show i)) -- single input case?
        (Function (Ft i o)) -> if es' == [i] then return o else (throwError (WrongType $ "Couldn't match type" ++ show es' ++ " with type " ++ show i))
exprtype (Binop Plus e1 e2) = do
  v1 <- exprtype e1
  v2 <- exprtype e2
  if (v1 == v2 && v1 == (Pext (X Itype []))) then return $ (Pext (X Itype [])) else throwError (WrongType "TYPE MISMATCH")
exprtype (Binop Equiv e1 e2) = do
  t1 <- exprtype e1
  t2 <- exprtype e2
  if (t1 == t2) then return (Pext (X Booltype [])) else throwError (WrongType $ "Couldn't match types " ++ show t1 ++ ", " ++ show t2)

extract (Pext x) = x
extract _ = undefined

getType :: String -> Typechecked Type
getType n = do
  env <- ask
  case lookup n env of
    Just e -> return e
    Nothing -> throwError (NotBound $ "not bound")

signatures :: [ValDef] -> Env
signatures = map f
  where f (Val (Sig n t) eq) = (n, t)

runTypeCheck :: Env -> ValDef -> Either TypeError Type
runTypeCheck e v = runIdentity $ runExceptT $ runReaderT (deftype v) e

tc :: Game -> IO Bool
tc (Game _ _ _ vs) = if all (isRight) (checked) then return True else ((putStrLn . show) $ lefts checked) >> return False
  where
    env = signatures vs
    checked = map (runTypeCheck env) vs




ex = Val (Sig "add"
          (Function (Ft
           (Pt (Tup [X Itype [], X Itype [], X Itype []]))
           (Pext (X Itype []))))) (Feq "addThenExpPlusOne" (Pars ["x", "y", "z"])
                                   (App "succ" [(App "exp"
                                    [(Binop Plus (Ref "x") (Ref "y")), (Ref "z")])]))
env :: Env
env = [("succ", (Function (Ft
           (Pext (X Itype []))
           (Pext (X Itype []))))),
        ("exp", (Function (Ft
           (Pt (Tup [(X Itype []), (X Itype [])]))
           (Pext (X Itype [])))))]

