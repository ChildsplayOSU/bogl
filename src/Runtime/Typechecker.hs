-- | Typechecker.

module Runtime.Typechecker (tc, tcexpr, signatures) where

import Language.Syntax
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.Except
import Debug.Trace
import Data.Either
import Data.Maybe

type Env = [(String, Type)]

-- | Encoding the different type errors as types should let us do interesting things with them
data TypeError = Mismatch Type Type -- ^ Couldn't match two types in an expression
               | NotBound Name -- ^ Name isn't bound in the enviroment
               | SigMismatch Name Type Type -- ^ couldn't match the type of an equation with its signature
               | Unknown String -- ^ Errors that "shouldn't happen"
               | BadOp Op Type Type

-- | smart constructors for type errors
mismatch :: Type -> Type -> Typechecked a
mismatch t1 t2 = throwError $ Mismatch t1 t2
notbound :: Name -> Typechecked a
notbound n = throwError $ NotBound n
sigmismatch :: Name -> Type -> Type -> Typechecked a
sigmismatch n t1 t2= throwError $ SigMismatch n t1 t2
unknown :: String -> Typechecked a
unknown s = throwError $ Unknown s
badop :: Op -> Type -> Type -> Typechecked a
badop o t1 t2 = throwError $ BadOp o t1 t2


instance Show TypeError where
  show (Mismatch t1 t2) = "Could not match types " ++ show t1 ++ " and " ++ show t2
  show (NotBound n) = "Variable " ++ n ++ " not bound in the enviroment!"
  show (SigMismatch n sig t) = "Signature for defition " ++ n ++ ": " ++ show sig ++ "\n does not match inferred type: " ++ show t
  show (Unknown s) = s
  show (BadOp o t1 t2) = "Cannot '" ++ show o ++ "' types " ++ show t1 ++ " and " ++ show t2

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
  if eqt == t then return t else sigmismatch n t eqt
-- | Get the type of an equation
eqntype :: Type -> Equation -> Typechecked Type
eqntype _ (Veq _ e) = exprtype e >>= (return . Plain)
eqntype (Function (Ft inputs _)) (Feq _ (Pars params) e) = do
    i <- return $ case inputs of
                t@(Pt (Tup ts)) -> detuple t
                x -> [Plain x]
    e' <- local ((++) (zip params (i))) (exprtype e)
    return $ Function (Ft inputs (e'))
eqntype _ _ = throwError (Unknown "Enviroment corrupted.") -- this should never happen?


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
    other -> unknown $ "Type " ++ show other ++ " is a function type and cannot be dereferenced."
exprtype (Tuple xs) = do
  xs' <- mapM exprtype xs
  return $ (Pt (Tup (map extract xs')))
exprtype (App n es) = do
  es' <- mapM exprtype es
  t <- getType n
  case t of
    (Function (Ft (Pt (Tup i)) o)) -> if map extract es' == i then return o else do
      ts <- mapM xtype es'
      mismatch (Plain $ Pt $ Tup ts) (Plain (Pt $ Tup i))
    (Function (Ft i o)) -> if es' == [i] then return o else do
      ts <- mapM xtype es' -- ugly indentation
      mismatch (Plain $ Pt $ Tup ts) (Plain i) -- thinking emoji FIXME
    _ -> do
      ts <- mapM xtype es'
      mismatch (Function $ (Ft (Pt (Tup ts)) (Pext (X Undef [])))) t -- TODO Get expected output from enviroment (fill in Undef what we know it should be)


                                      
exprtype (Binop Equiv e1 e2) = do
  t1 <- exprtype e1
  t2 <- exprtype e2
  if (t1 == t2) then return (Pext (X Booltype [])) else badop Equiv (Plain t1) (Plain t2)
exprtype (Binop x e1 e2) = do
  t1 <- exprtype e1
  t2 <- exprtype e2
  case (t1, t2) of
    (Pext (X Itype []), Pext (X Itype [])) -> if x `elem` [Plus, Minus, Times, Div, Mod]
                                              then return $ (Pext (X Itype []))
                                              else badop x (Plain t1) (Plain t2)
    (Pext (X Booltype []), Pext (X Booltype [])) -> if x `elem` [And, Or, Xor]
                                                    then return $ (Pext (X Booltype []))
                                                    else badop x (Plain t1) (Plain t2)
    _ -> badop x (Plain t1) (Plain t2)


-- if
exprtype (If e1 e2 e3) = undefined
-- while
exprtype (While n1 n2 e) = undefined

xtype :: Ptype -> Typechecked Xtype
xtype (Pext x) = return x
xtype y = mismatch (Plain (Pext (X Undef []))) (Plain y)

-- | Extract a type
extract (Pext x) = x
extract _ = undefined

-- | Get the type of a reference in the enviroment
getType :: String -> Typechecked Type
getType n = do
  env <- ask
  case lookup n env of
    Just e -> return e
    Nothing -> notbound n

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

