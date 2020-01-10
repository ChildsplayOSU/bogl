-- |

module Runtime.Typechecker (tc, tcexpr, signatures) where

import Language.Syntax
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.Except
import Debug.Trace
import Data.Either
type Error = String
type Env = [(String, Type)]

-- | You've either given me a wrong type, or its a name that's not bound
data TypeError = WrongType String | NotBound String
  deriving Show

-- | Things are typechecked with an enviroment ('ReaderT') and the possibility of failure ('ExceptT'). The typechecker is non-interactive so we do not need IO.
type Typechecked a =  (ReaderT Env (ExceptT TypeError Identity)) a

-- | Deconstruct a tuple type into a list of types
detuple :: Ptype -> [Type]
detuple (Pt (Tup ts)) = map Plain (map Pext ts)
detuple x = (trace $ show x) $ undefined

-- | Get the type of a valDef. Check the expression's type with the signature's. If they don't match, throw exception.
deftype :: ValDef -> Typechecked Type
deftype (Val (Sig n t) eqn) = do
  eqt <- eqntype t eqn
  if eqt == t then return t else throwError $ WrongType $ "Type " ++ show eqt ++ " doesn't match signature" ++ show t

-- | Get the type of an equation
eqntype :: Type -> Equation -> Typechecked Type
eqntype _ (Veq _ e) = exprtype e >>= (return . Plain)
eqntype (Function (Ft inputs _)) (Feq _ (Pars params) e) = do
    i <- return $ case inputs of
                t@(Pt (Tup ts)) -> detuple t
                x -> [Plain x]
    e' <- local ((++) (zip params (i))) (exprtype e)
    return $ Function (Ft inputs (e'))
eqntype _ _ = throwError (WrongType "ERR!")


-- Get the type of an expression
exprtype :: Expr -> Typechecked Ptype
exprtype (I _) = return $ (Pext (X Itype []))
exprtype (S s) = return $ (Pext (X (Symbol s) []))
exprtype (B _) = return $ (Pext (X Booltype []))
exprtype (Let n e1 e2) = do
  t <- exprtype e1
  local ((n, Plain t):) (exprtype e2)
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
exprtype (Binop Equiv e1 e2) = do
  t1 <- exprtype e1
  t2 <- exprtype e2
  if (t1 == t2) then return (Pext (X Booltype [])) else throwError (WrongType $ "Couldn't match types " ++ show t1 ++ ", " ++ show t2)
exprtype (Binop x e1 e2) = do
  v1 <- exprtype e1
  v2 <- exprtype e2
  case (v1, v2) of
    (Pext (X Itype []), Pext (X Itype [])) -> if x `elem` [Plus, Minus, Times, Div, Mod]
                                              then return $ (Pext (X Itype []))
                                              else throwError (WrongType $ "Cannot " ++ show x ++ " types " ++ show v1 ++ " and " ++ show v2 )
    (Pext (X Booltype []), Pext (X Booltype [])) -> if x `elem` [And, Or, Xor]
                                                    then return $ (Pext (X Itype []))
                                                    else throwError (WrongType $ "Cannot " ++ show x ++ " types " ++ show v1 ++ " and " ++ show v2)
    (x, y) -> throwError (WrongType $ "Type mismatch: " ++ show x ++ " is not " ++ show y)


-- if
exprtype (If e1 e2 e3) = undefined
-- while
exprtype (While n1 n2 e) = undefined

-- | Extract a type
extract (Pext x) = x
extract _ = undefined

-- | Get the type of a reference in the enviroment
getType :: String -> Typechecked Type
getType n = do
  env <- ask
  case lookup n env of
    Just e -> return e
    Nothing -> throwError (NotBound $ "not bound")

-- | Get all the signatures out of the list of value defintions
signatures :: [ValDef] -> Env
signatures = map f
  where f (Val (Sig n t) eq) = (n, t)

-- | Run the typechecker on env and a list of ValDefs
runTypeCheck :: Env -> ValDef -> Either TypeError Type
runTypeCheck e v = runIdentity $ runExceptT $ runReaderT (deftype v) e

-- | Run the typechecker on a 'Game' and report any errors to the console.
tc :: Game -> IO Bool
tc (Game _ _ _ vs) = if all (isRight) (checked) then return True else ((putStrLn . show) $ lefts checked) >> return False
  where
    env = signatures vs
    checked = map (runTypeCheck env) vs

-- | Run the typechecker on an 'Expr' and report any erros to the console.
tcexpr :: Env -> Expr -> IO Bool
tcexpr e x = do
  if isRight t then return True else ((putStrLn . show) t) >> return False
  where
    t = runIdentity $ runExceptT $ runReaderT (exprtype x) e


-- | an example valdef
ex = Val (Sig "add"
          (Function (Ft
           (Pt (Tup [X Itype [], X Itype [], X Itype []]))
           (Pext (X Itype []))))) (Feq "addThenExpPlusOne" (Pars ["x", "y", "z"])
                                   (App "succ" [(App "exp"
                                    [(Binop Plus (Ref "x") (Ref "y")), (Ref "z")])]))
-- | an example enviroment
env :: Env
env = [("succ", (Function (Ft
           (Pext (X Itype []))
           (Pext (X Itype []))))),
        ("exp", (Function (Ft
           (Pt (Tup [(X Itype []), (X Itype [])]))
           (Pext (X Itype [])))))]

