{-|

Module      : Typechecker.Typechecker
Description : Implementation of the BoGL typechecker
Copyright   : (c)
License     : BSD-3
-}

module Typechecker.Typechecker (tcexpr, environment, runTypeCheck, tc, TcResult(..), showTCError, exprHasInputType) where

import Runtime.Builtins
import Language.Syntax hiding (input)
import Language.Types

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Extra

import Data.Either
import Data.List

import Typechecker.Monad
import Text.Parsec.Pos
import qualified Data.Set as S
import Utils.String

import Error.Error
--import Debug.Trace

-- | return Nothing or the first element of a list which doen't satisfy a predicate
all' :: Foldable t => (a -> Bool) -> t a -> Maybe a
all' _p xs = foldl (\none x -> if _p x then none else Just x) Nothing xs


-- | Return a list of the covered positions for a board equation
getCovered :: (Int,Int) -> BoardEq SourcePos -> [(Int,Int)]
getCovered (mx,my) (PosDef _ xp yp _) = case (xp,yp) of
                                  (ForAll _,ForAll _) -> [(x,y) | x <- [1..mx], y <- [1..my]] -- (x,y) all spaces
                                  (ForAll _, Index y) -> [(x,y) | x <- [1..mx]] -- (x,#) row of spaces
                                  (Index x, ForAll _) -> [(x,y) | y <- [1..my]] -- (#,y) column of spaces
                                  (Index x, Index y)  -> [(x,y)]  -- (#,#), single space


-- | Get the type of a valDef. Check the expression's type with the signature's.
--   If they don't match, throw exception.
deftype :: (ValDef SourcePos) -> Typechecked Type
deftype (Val (Sig n _t) eqn x) = do
  setPos x
  eqt <- localEnv ((n, _t):) (eqntype n _t eqn)
  ifM (eqt <: _t) (return _t) (sigmismatch n _t eqt)

deftype (BVal (Sig n _t) eqs x) = do
  setPos x
  (mx, my) <- getSize
  -- get a set of the spaces covered by these board equations
  let placesCovered = S.fromList $ concat $ map (getCovered (mx,my)) eqs
  eqTypes  <- mapM beqntype eqs
  allSubs  <- mapM (<: _t) eqTypes
  case all' (\(_, isSub) -> isSub) (zip eqTypes allSubs) of
    -- pass if all spaces are defined, otherwise incomplete/uninitialized
    -- placesCovered is a set of all places. Places of invalid positions do not count
    -- i.e, if the size of the set of positions is equivalent to mx*my, then all correct positions have been covered
    Nothing -> if length placesCovered == mx*my then return _t else uninitialized n
    (Just (badEqn, _)) -> sigmismatch n _t badEqn

-- | Get the type of a board equation
beqntype :: BoardEq SourcePos -> Typechecked Type
beqntype (PosDef _ xp yp _e) = do
   et <- exprtypeE _e
   pt <- getContent
   isSub <- et <: pt
   (mx, my) <- getSize
   case (isSub, xp <= Index mx && xp > Index 0, yp <= Index my && yp > Index 0) of
      (True, True, True) -> return boardt
      (False, _, _)      -> mismatch (Plain pt) (Plain et)
      _                  -> outofbounds xp yp

-- | Get the type of an equation
eqntype :: Name -> Type -> (Equation SourcePos) -> Typechecked Type
eqntype _ _ (Veq _ _e) = exprtypeE _e >>= (return . Plain)
eqntype n t'@(Function (Ft inputs _)) (Feq _ (Pars params) _e) = do
  it <- findTuple inputs
  case it of
    (Tup inputs') -> do
      e' <- localEnv ((++) (zip params (map Plain inputs'))) (exprtypeE _e)
      checkLen inputs' params (Function (Ft inputs e'))
    (input') -> do
      e' <- localEnv ((++) (zip params (pure (Plain input')))) (exprtypeE _e)
      checkLen [input'] params (Function (Ft inputs e'))
  where
    checkLen a b c = let la = length a
                         lb = length b in
                     if la == lb then
                      return c
                     else
                       unknown $ "The equation for " ++ quote n ++ " has " ++ (show lb) ++ " parameters, but its type " ++ (show t') ++ " has only " ++ (show la)

eqntype n et f = sigbadfeq n et $ (clearAnnEq f)

-- | Synthesize the type of an expression
exprtypeE :: (Expr SourcePos) -> Typechecked Xtype -- TODO do this with mapStateT stack thing
exprtypeE _e = setSrc _e >> exprtype _e

-- | Assign a type to a symbol based on the first type definition that it appears in
assignSymbol :: Name -> Typechecked Xtype
assignSymbol n = do
                    ds   <- getDefs
                    case find (\a -> declaredIn n (snd a)) ds of
                       (Just (tn, _)) -> return $ namedt tn
                       _             -> unknown $ "The value " ++ n ++ " does not have a type"
   where
      -- TODO! works only if type defs are stored in program order
      -- in the formal spec, symbol -> type name mappings are stored in the type env
      declaredIn s (X _ ss) = S.member s ss
      declaredIn _ _       = False

-- | Get the type of an expression
exprtype :: (Expr SourcePos) -> Typechecked Xtype
exprtype (Annotation a _e) = setPos a >> exprtype _e
exprtype (I _) = t Itype
exprtype (S s) = assignSymbol s
exprtype (B _) = t Booltype
exprtype (Let n e1 e2) = do
  _t <- exprtype e1
  localEnv ((n, Plain _t):) (exprtype e2)
exprtype (Ref s) = do
  x <- getType s
  case x of
    (Plain _t) -> return _t
    other -> dereff s other
exprtype (Tuple xs) = do
  xs' <- mapM exprtype xs
  return $ Tup xs'
exprtype (App n es) = do
  est <- exprtype es
  _t <- getType n
  case _t of
    (Function (Ft i o)) -> do
     argsMatch <- est <: i
     if argsMatch then
        return o
     else
        appmismatch n (Plain i) (Plain est)
    _ -> do
      badapp n es
exprtype (Binop Get e1 (Annotation _ (Tuple [Annotation _ (I x), Annotation _ (I y)]))) =
   exprtype (Binop Get e1 (Tuple [I x, I y]))
exprtype (Binop Get e1 (Tuple [(I x), (I y)])) = do
  _t1 <- exprtype e1
  inB <- inBounds (x, y)
  hasType _t1 boardxt
  if inB
     then getContent
     else outofbounds (Index x) (Index y)
exprtype (Binop Get e1 e2) = do
  _t1 <- exprtype e1
  _t2 <- exprtype e2
  hasType _t1 (X Board S.empty)
  hasType _t2 (Tup [intxt, intxt])
  getContent
exprtype (Binop o e1 e2) = do
  _t1 <- exprtype e1
  _t2 <- exprtype e2
  t' <- unify _t1 _t2
  isInt <- t' <: intxt
  case (isInt, arithmetic o, relational o, equiv o) of
     (True, True, _, _)      -> return intxt
     (True, False, True, _)  -> return boolxt
     (_, False, False, True) -> return boolxt
     _                       -> badop o (Plain _t1) (Plain _t2)
exprtype (If e1 e2 e3) = do
  _t1 <- exprtype e1
  _t2 <- exprtype e2
  t3 <- exprtype e3
  ifM (_t1 <: boolxt) (unify _t2 t3) errMsg
  where
     errMsg = unknown $ "The condition for 'if' must be of type " ++ show boolxt
exprtype (While c b _ _e) = do
  et <- exprtype _e
  ct <- exprtype c
  bt <- exprtype b
  let report = ifM (bt <: et) (mismatch (Plain ct) (Plain boolxt)) (mismatch (Plain bt) (Plain et))
  ifM ((ct <: boolxt) &&^ (bt <: et)) (return et) report

-- | Produce the environment
environment :: BoardDef -> InputDef -> [ValDef SourcePos] -> [TypeDef] -> Env
environment (BoardDef sz _t) (InputDef i) vs td = Env (map f vs ++ (builtinT i _t)) td i _t sz
  where f (Val (Sig n _t1) _ _)  = (n, _t1)
        f (BVal (Sig n _t1) _ _) = (n, _t1)

-- | Runs the typechecker on a board def, input def, list of valdefs, and produces results of errors or successfully typechecked names
runTypeCheck :: BoardDef -> InputDef -> [ValDef SourcePos] -> [TypeDef]-> Writer [Either (ValDef SourcePos, Error) (Name, Type)] Env
runTypeCheck (BoardDef sz _t) (InputDef i) vs td = foldM (\env v -> case typecheck env (deftype v) of
                                Right (_t2, _e) -> (return $ extendEnv env (ident v, _t2))
                                Left _err       -> (tell . pure . Left) (v, _err) >> return env)
                                    (initEnv i _t sz td)
                                    (vs)

-- | Typechecker Result
data TcResult =
  Tc {
       success :: Bool,                       -- ^ Success result
       e :: Env,                              -- ^ Env associated with the result
       errors :: [(ValDef SourcePos, Error)], -- ^ Typechecker errors
       rtypes :: [(Name, Type)]               -- ^ List of (name,type) pairs
     }

-- | Produces an error corresponding to a typechecker error
showTCError :: (ValDef SourcePos, Error) -> String
showTCError (_p, _e) = (show _e) ++ "\n" ++ show _p

-- | Typecheck a game and produce a typechecker result
tc :: (Game SourcePos) -> TcResult
tc g = case tc' g of
  (_e, ls) -> let l = lefts ls in
    Tc (length l == 0) _e l (rights ls ++ types _e)

-- | Typecheck a game, and produce (environment, result of typechecking)
tc' :: (Game SourcePos) -> (Env, [Either (ValDef SourcePos, Error) (Name, Type)])
tc' (Game _ b i v td) = runWriter (runTypeCheck b i v td)

-- | Check if a given 'Expr' is a subtype of Input
exprHasInputType :: Env -> (Expr SourcePos) -> Either Error ((), Stat)
exprHasInputType tcenv = typecheck tcenv . isInputType

-- | Check if a given 'Expr' is a subtype of Input
isInputType :: (Expr SourcePos) -> Typechecked ()
isInputType ie = do
   et <- exprtypeE ie
   it <- getInput
   isSub <- et <: it
   if isSub then return () else inputmismatch $ Plain et

-- | Run the typechecker on an 'Expr' and report any errors to the console.
tcexpr :: Env -> (Expr SourcePos) -> Either Error (Xtype, Stat)
tcexpr _e x = typecheck _e (exprtypeE x)
