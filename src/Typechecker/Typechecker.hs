-- | Typechecker.
-- todo emit types of expressions
module Typechecker.Typechecker (tcexpr, environment, runTypeCheck, tc, printT) where

import Runtime.Builtins
import Language.Syntax hiding (input, piece, size)

import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.Writer
import Control.Monad.Reader

import Debug.Trace
import Data.Either
import Data.Maybe
import Data.Bifunctor
import Data.List

import Typechecker.Monad

import qualified Data.Set as S

-- | Get the type of a valDef. Check the expression's type with the signature's. If they don't match, throw exception.
deftype :: ValDef -> Typechecked Type
deftype (Val (Sig n t) eqn) = do
  eqt <- localEnv ((n, t):) (eqntype t eqn)
  if eqt <= t
    then return t
    else sigmismatch n t eqt

deftype (BVal (Sig n t) eqn) = do
  eqt <- beqntype t eqn
  if eqt <= t
    then return t
    else sigmismatch n t eqt

-- | Get the type of a board equation.
beqntype :: Type -> BoardEq -> Typechecked Type
beqntype t (PosDef _ xp yp e) = do
   t1 <- exprtype e
   b <- getPiece
   sz <- getSize
   unify t1 b
   case (t1 <= b, toPos sz >= (xp,yp)) of
     (True, True) -> return $ Plain (X Board S.empty)
     (False, _) -> mismatch (Plain b) (Plain t1)
     (_, False) -> outofbounds xp yp
   where
     toPos (x,y) = (Index x, Index y) -- fixme
-- | Get the type of an equation
eqntype :: Type -> Equation -> Typechecked Type
eqntype _ (Veq _ e) = exprtypeE e >>= (return . Plain)
eqntype (Function (Ft inputs _)) (Feq _ (Pars params) e) = do
  case inputs of
    (Tup inputs') -> do
      e' <- localEnv ((++) (zip params (map Plain inputs'))) (exprtype e)
      return $ Function (Ft inputs e')
    (input') -> do
      e' <- localEnv ((++) (zip params (pure (Plain input')))) (exprtype e)
      return $ Function (Ft inputs e')
  where
eqntype _ _ = throwError (Unknown "Environment corrupted.") -- this should never happen?

-- Synthesize the type of an expression
exprtypeE :: Expr -> Typechecked Xtype -- TODO do this with mapStateT stack thing
exprtypeE e = setSrc e >> exprtype e
exprtype :: Expr -> Typechecked Xtype
exprtype (I _) = t Itype
exprtype (S s) = return $ X Top (S.singleton s)

exprtype (B _) = t Booltype
exprtype (Let n e1 e2) = do
  t <- exprtype e1
  localEnv ((n, Plain t):) (exprtype e2)
exprtype (Ref s) = do
  x <- getType s
  case x of
    (Plain t) -> return t
    other -> unknown $ "Object " ++ s ++ " of type " ++ show other ++ " is a function and cannot be dereferenced."
exprtype (Tuple xs) = do
  xs' <- mapM exprtype xs
  return $ Tup xs'
exprtype e@(App n es) = do -- FIXME. Tuple composition is bad.
  es' <- mapM exprtype es
  let es'' = foldr (\x k -> case x of
        (Tup xs) -> xs ++ k
        xs -> xs:k) [] es' -- oof
  t <- getType n
  case t of
    (Function (Ft (i) o)) -> do
      unify (Tup es'') i -- oof
      return o
    _ -> do
      (traceM "???") >> mismatch (Function $ (Ft (Tup es') (X Undef S.empty))) t -- TODO Get expected output from enviroment (fill in Undef what we know it should be)
exprtype e@(Binop Equiv e1 e2) = do
  t1 <- exprtype e1
  t2 <- exprtype e2
  unify t1 t2
  t Booltype
exprtype (Binop Get e1 e2) = do
  t1 <- exprtype e1
  t2 <- exprtype e2
  unify t1 (ext Board)
  unify t2 (ext Position)
  getPiece
exprtype (Binop x e1 e2) = do
  t1 <- exprtype e1
  t2 <- exprtype e2
  t' <- unify t1 t2
  case (t') of
    (X Itype s1) | S.null s1 -> if x `elem` [Plus, Minus, Times, Div, Mod]
                                              then t Itype
                                              else if x `elem` [Less, Greater]
                                                   then t Booltype
                                                   else badop x (Plain t1) (Plain t2)
    (X Booltype s1) -> if x `elem` [And, Or, Xor] && S.null s1
                                                    then t Booltype
                                                    else badop x (Plain t1) (Plain t2)
    _ -> badop x (Plain t1) (Plain t2)
-- if
exprtype (If e1 e2 e3) = do
  t1 <- exprtype e1
  t2 <- exprtype e2
  t3 <- exprtype e3
  b <- unify t1 (ext Booltype)
  case b of
    (X Booltype empty) -> unify t2 t3
  {- case (t1, t2, t3) of
    ((X Booltype empty), (Tup xs), (Tup ys)) | S.null empty -> do
                                                 result <- forM (zip xs ys) (\(x, y) -> mergeX x y)
                                                 return (Tup result) -- this is strange.
    ((X Booltype empty), t2, t3) | S.null empty  -> mergeX t2 t3
    (x, y, z) -> traceM (show x ++ " " ++ show y ++ show z) >> (mismatch (Plain $ (X Booltype S.empty)) (Plain x) e) -}
exprtype (HE n) = return (Hole n)

exprtype e'@(While c b n e) = do
  et <- exprtype e
  ct <- exprtype c
  bt <- exprtype b
  case (ct, bt) of
    ((X Booltype s), y) | S.null s && y == et -> return et
    (a, b) -> if b == et
              then mismatch (Plain b) (Plain (X Booltype S.empty))
              else mismatch (Plain a) (Plain et)



-- | Produce the environment
environment :: BoardDef -> InputDef -> [ValDef] -> Env
environment (BoardDef sz t) (InputDef i) vs = Env (map f vs ++ builtinT) i t sz
  where f (Val (Sig n t1) eq) = (n, t1)
        f (BVal (Sig n t1) eq) = (n, t1)

-- recursion is not allowed by this.
runTypeCheck :: BoardDef -> InputDef -> [ValDef] -> Writer [Either (ValDef, TypeError) (Name, Type)] Env
runTypeCheck (BoardDef sz t) (InputDef i) vs = foldM (\env v -> case typecheck env (deftype v) of
                                Right (t, e) -> tell (map Right (e)) >> (return $ extendEnv env (ident v, t))
                                Left err -> (tell . pure . Left) (v, err) >> return env)
                                    (initEnv i t sz)
                                    (vs)

tc :: Game -> (Env, [Either (ValDef, TypeError) (Name, Type)])
tc (Game n b i v) = runWriter (runTypeCheck b i v)

-- | Run the typechecker on an 'Expr' and report any errors to the console.
tcexpr :: Env -> Expr -> Either TypeError (Xtype, TypeEnv)
tcexpr e x = typecheck e (exprtypeE x)

printT :: (Xtype, TypeEnv) -> String
printT (x, env) = show x ++ "\n" ++ "Type Holes:" ++ (intercalate "\n" (map (\(a, b) -> a ++ ": " ++ show b) env))
