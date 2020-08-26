{-|
Module      : Typechecker.Typechecker
Description : Implementation of the BoGL typechecker
Copyright   : (c)
License     : BSD-3
-}

module Typechecker.Typechecker (tcexpr, environment, runTypeCheck, tc, printT, TcResult(..), showTCError) where

import Runtime.Builtins
import Language.Syntax hiding (input, piece, size)
import Language.Types

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
import Text.Parsec.Pos
import qualified Data.Set as S

-- | return Nothing or the first element of a list which doen't satisfy a predicate
all' p xs = foldl (\none x -> if p x then none else Just x) Nothing xs

-- | Get the type of a valDef. Check the expression's type with the signature's.
--   If they don't match, throw exception.
deftype :: (ValDef SourcePos) -> Typechecked Type
deftype (Val (Sig n t) eqn x) = do
  setPos x
  eqt <- localEnv ((n, t):) (eqntype t eqn)
  if eqt <= t
    then return t
    else sigmismatch n t eqt

deftype (BVal (Sig n t) eqs x) = do
  setPos x
  eqTypes <- mapM beqntype eqs
  case all' (<= t) eqTypes of
    Nothing -> return t
    (Just badEqn) -> sigmismatch n t badEqn

-- | Get the type of a board equation
beqntype :: BoardEq SourcePos -> Typechecked Type
beqntype (PosDef n xp yp e) = do
   et <- exprtype e
   pt <- getPiece
   (mx, my) <- getSize
   case (et <= pt, xp <= Index mx && xp > Index 0, yp <= Index my && yp > Index 0) of
      (True, True, True) -> return boardt
      (False, _, _)      -> mismatch (Plain pt) (Plain et)
      _                  -> outofbounds xp yp

-- | Get the type of an equation
eqntype :: Type -> (Equation SourcePos) -> Typechecked Type
eqntype _ (Veq _ e) = exprtypeE e >>= (return . Plain)
eqntype (Function (Ft inputs _)) (Feq _ (Pars params) e) = do
  case inputs of
    (Tup inputs') -> do
      e' <- localEnv ((++) (zip params (map Plain inputs'))) (exprtypeE e)
      return $ Function (Ft inputs e')
    (input') -> do
      e' <- localEnv ((++) (zip params (pure (Plain input')))) (exprtypeE e)
      return $ Function (Ft inputs e')
  where
eqntype _ _ = throwError (Unknown "Environment corrupted." undefined) -- this should never happen?

-- Synthesize the type of an expression
exprtypeE :: (Expr SourcePos) -> Typechecked Xtype -- TODO do this with mapStateT stack thing
exprtypeE e = setSrc e >> exprtype e

-- | Get the type of an expression
exprtype :: (Expr SourcePos) -> Typechecked Xtype
exprtype (Annotation a e) = setPos a >> exprtype e
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
    other -> dereff s other
exprtype (Tuple xs) = do
  xs' <- mapM exprtype xs
  return $ Tup xs'
exprtype e@(App n es) = do -- FIXME. Tuple composition is bad.
  es' <- exprtype es
  let es'' = case es' of
        Tup [Tup xs] -> Tup xs
        x -> x
  t <- getType n
  case t of
    (Function (Ft i o)) -> do
      if es'' == i then
        return o -- supplied param matches expected input type
      else
        do
          -- otherwise, unify the supplied param w/ the input type
          u <- unify (es'') i -- oof
          case u of
            -- verify the unified result is ultimately the same as the input type
            x1 -> if x1 == i then return o else mismatch (Plain es'') (Plain i)
    _ -> do
      badapp n es
exprtype (Binop Get e1 (Annotation _ (Tuple [Annotation _ (I x), Annotation _ (I y)]))) =
   exprtype (Binop Get e1 (Tuple [I x, I y]))
exprtype (Binop Get e1 (Tuple [(I x), (I y)])) = do
  t1 <- exprtype e1
  unify t1 (X Board S.empty)
  inB <- inBounds (x, y)
  if inB
   then getPiece
   else outofbounds (Index x) (Index y)
exprtype (Binop Get e1 e2) = do
  t1 <- exprtype e1
  t2 <- exprtype e2
  unify t1 (X Board S.empty)
  unify t2 (Tup [X Itype S.empty, X Itype S.empty])
  getPiece
exprtype (Binop o e1 e2) = do
  t1 <- exprtype e1
  t2 <- exprtype e2
  t' <- unify t1 t2
  case (t' == intxt, arithmetic o, relational o, equiv o) of
     (True, True, _, _)      -> return intxt
     (True, False, True, _)  -> return boolxt
     (_, False, False, True) -> return boolxt
     _                       -> badop o (Plain t1) (Plain t2)
exprtype (If e1 e2 e3) = do
  t1 <- exprtype e1
  t2 <- exprtype e2
  t3 <- exprtype e3
  b <- unify t1 (X Booltype S.empty)
  case b of
    (X Booltype empty) -> unify t2 t3
exprtype (HE n) = return (Hole n)
exprtype (While c b n e) = do
  et <- exprtype e
  ct <- exprtype c
  bt <- exprtype b
  case (ct, bt) of
    ((X Booltype s), y) | S.null s && y == et -> return et
    (a, b) -> if b == et
              then mismatch (Plain b) (Plain (X Booltype S.empty))
              else mismatch (Plain a) (Plain et)

-- | Produce the environment
environment :: BoardDef -> InputDef -> [ValDef SourcePos] -> Env
environment (BoardDef sz t) (InputDef i) vs = Env (map f vs ++ (builtinT i t)) i t sz
  where f (Val (Sig n t1) eq x) = (n, t1)
        f (BVal (Sig n t1) eq x) = (n, t1)

-- | Runs the typechecker on a board def, input def, list of valdefs, and produces results of errors or successfully typechecked names
-- recursion is not allowed by this.
runTypeCheck :: BoardDef -> InputDef -> [ValDef SourcePos] -> Writer [Either (ValDef SourcePos, TypeError) (Name, Type)] Env
runTypeCheck (BoardDef sz t) (InputDef i) vs = foldM (\env v -> case typecheck env (deftype v) of
                                Right (t, e) -> tell (map Right (holes e)) >> (return $ extendEnv env (ident v, t))
                                Left err -> (tell . pure . Left) (v, err) >> return env)
                                    (initEnv i t sz)
                                    (vs)

-- | Typechecker Result
data TcResult =
  Tc {
  success :: Bool,  -- ^ Success result
  e :: Env,         -- ^ Env associated with the result
  errors :: [(ValDef SourcePos, TypeError)],  -- ^ Typechecker errors
  rtypes :: [(Name, Type)]  -- ^ List of (name,type) pairs
     }

-- | Produces an error corresponding to a typechecker error
showTCError :: (ValDef SourcePos, TypeError) -> String
showTCError (p, e) = (show e) ++ "\n" ++ show p

-- | Typecheck a game and produce a typechecker result
tc :: (Game SourcePos) -> TcResult
tc g = case tc' g of
  (e, ls) -> let l = lefts ls in
    Tc (length l == 0) e l (rights ls ++ types e)

-- | Typecheck a game, and produce a tuple of an environment, with a list of errors and/or successfully typechecked names
tc' :: (Game SourcePos) -> (Env, [Either (ValDef SourcePos, TypeError) (Name, Type)])
tc' (Game n b i v) = runWriter (runTypeCheck b i v)

-- | Run the typechecker on an 'Expr' and report any errors to the console.
tcexpr :: Env -> (Expr SourcePos) -> Either TypeError (Xtype, TypeEnv)
tcexpr e x = typeHoles e (exprtypeE x)

-- | Produce a string of types in the environment
printT :: (Xtype, TypeEnv) -> String
printT (x, env) = show x ++ "\n" ++ "Type Holes:" ++
                  (intercalate "\n" (map (\(a, b) -> a ++ ": " ++ show b) env))
