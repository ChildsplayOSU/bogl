-- | Typechecker.

module Runtime.Typechecker (tc, tcexpr, signatures) where

import Language.Syntax
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.Except
import Debug.Trace
import Data.Either
import Data.Maybe
import qualified Data.Set as S

type Env = [(String, Type)]

-- | Encoding the different type errors as types should let us do interesting things with them
data TypeError = Mismatch Type Type Expr -- ^ Couldn't match two types in an expression
               | NotBound Name -- ^ Name isn't bound in the enviroment
               | SigMismatch Name Type Type -- ^ couldn't match the type of an equation with its signature
               | Unknown String -- ^ Errors that "shouldn't happen"
               | BadOp Op Type Type Expr

-- | smart constructors for type errors
mismatch :: Type -> Type -> Expr -> Typechecked a
mismatch t1 t2 e = throwError $ Mismatch t1 t2 e
notbound :: Name -> Typechecked a
notbound n = throwError $ NotBound n
sigmismatch :: Name -> Type -> Type -> Typechecked a
sigmismatch n t1 t2= throwError $ SigMismatch n t1 t2
unknown :: String -> Typechecked a
unknown s = throwError $ Unknown s
badop :: Op -> Type -> Type -> Expr -> Typechecked a
badop o t1 t2 e = throwError $ BadOp o t1 t2 e


instance Show TypeError where
  show (Mismatch t1 t2 e) = "Could not match types " ++ show t1 ++ " and " ++ show t2 ++ "\n in expression: " ++ show e
  show (NotBound n) = "Variable " ++ n ++ " not bound in the enviroment!"
  show (SigMismatch n sig t) = "Signature for defition " ++ n ++ ": " ++ show sig ++ "\n does not match inferred type: " ++ show t
  show (Unknown s) = s
  show (BadOp o t1 t2 e) = "Cannot '" ++ show o ++ "' types " ++ show t1 ++ " and " ++ show t2 ++ "\n in expression: " ++ show e

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
exprtype (I _) = return $ (Pext (X Itype S.empty))
exprtype (S s) = return $ (Pext (X (Symbol s) S.empty))
exprtype (B _) = return $ (Pext (X Booltype S.empty))
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
exprtype e@(App n es) = do
  es' <- mapM exprtype es
  t <- getType n
  case t of
    (Function (Ft (Pt (Tup i)) o)) -> if map extract es' == i then return o else do
      ts <- mapM (xtype e) es'
      mismatch (Plain $ Pt $ Tup ts) (Plain (Pt $ Tup i)) e
    (Function (Ft i o)) -> if es' == [i] then return o else do
      ts <- mapM (xtype e) es'
      mismatch (Plain $ Pt $ Tup ts) (Plain i) e
    _ -> do
      ts <- mapM (xtype e) es'
      mismatch (Function $ (Ft (Pt (Tup ts)) (Pext (X Undef S.empty)))) t e -- TODO Get expected output from enviroment (fill in Undef what we know it should be)


                                      
exprtype e@(Binop Equiv e1 e2) = do
  t1 <- exprtype e1
  t2 <- exprtype e2
  if (t1 == t2) then return (Pext (X Booltype S.empty)) else badop Equiv (Plain t1) (Plain t2) e
exprtype e@(Binop x e1 e2) = do
  t1 <- exprtype e1
  t2 <- exprtype e2
  case (t1, t2) of
    (Pext (X Itype s1), Pext (X Itype s2)) -> if x `elem` [Plus, Minus, Times, Div, Mod] && S.null s1 && S.null s2
                                              then return $ (Pext (X Itype S.empty))
                                              else badop x (Plain t1) (Plain t2) e
    (Pext (X Booltype s1), Pext (X Booltype s2)) -> if x `elem` [And, Or, Xor] && S.null s1 && S.null s2
                                                    then return $ (Pext (X Booltype S.empty))
                                                    else badop x (Plain t1) (Plain t2) e
    _ -> badop x (Plain t1) (Plain t2) e


-- if
exprtype e@(If e1 e2 e3) = do
  t1 <- exprtype e1
  t2 <- exprtype e2
  t3 <- exprtype e3
  case (t1, t2, t3) of
    (Pext (X Booltype empty), Pext (X (Symbol s) s1), Pext (X t3' s2)) | S.null empty -> return $ Pext (X t3' $ S.unions [s1, s2, S.fromList [s]])
    (Pext (X Booltype empty), Pext ((X t2' s1)), Pext (X (Symbol s) s2)) | S.null empty -> return $ Pext (X t2' $ S.unions [s1, s2, S.fromList [s]])
    (Pext (X Booltype empty), y, z) | S.null empty -> mismatch (Plain y) (Plain z) e
    (x, _, _) -> mismatch (Plain $ Pext $ (X Booltype S.empty)) (Plain x) e


-- while
exprtype (While n1 n2 e) = undefined

xtype :: Expr -> Ptype -> Typechecked Xtype
xtype e (Pext x) = return x
xtype e y = mismatch (Plain (Pext (X Undef S.empty))) (Plain y) e

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


