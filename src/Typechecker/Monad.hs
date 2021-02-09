{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Typechecker.Monad
Description : Typechecker monad
Copyright   : (c)
License     : BSD-3
-}

module Typechecker.Monad where

import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Extra
import Text.Parsec.Pos


import Language.Types hiding (content, size)

import qualified Data.Set as S

import Language.Syntax hiding (input)
import Runtime.Builtins

import Error.Error
import Error.TypeError

import Data.List

import Debug.Trace

-- | Types in the environment
type TypeEnv = [(Name, Type)]

-- | Typechecker environment
data Env = Env {
  types :: TypeEnv,
  defs  :: [TypeDef],
  input :: Xtype,
  content :: Xtype,
  size  :: (Int, Int)
               }

-- | Initial empty environment
initEnv :: Xtype -> Xtype -> (Int, Int) -> [TypeDef] -> Env
initEnv i _p s td = Env [] td i _p s

-- | An example environment for interal use (e.g. testing, ghci)
exampleEnv :: Env
exampleEnv = Env (builtinT intxt intxt) [] intxt intxt (5, 5)

-- | Typechecker state
data Stat = Stat {
  source :: Maybe (Expr SourcePos),
  pos :: SourcePos
            }

-- | Typechecking monad
type Typechecked a = (StateT Stat (ReaderT Env (ExceptT Error Identity))) a

-- | Run a computation inside of the typechecking monad
typecheck :: Env -> Typechecked a -> Either Error (a, Stat)
typecheck e a = runIdentity . runExceptT . (flip runReaderT e) $
                (runStateT a (Stat Nothing (newPos "" 0 0)))

-- | Add some types to the environment
extendEnv :: Env -> (Name, Type) -> Env
extendEnv (Env _t d i _p s) v = Env (v:_t) d i _p s

-- | Get the type environment
getEnv :: Typechecked TypeEnv
getEnv = types <$> ask

-- | Get the type definitions
getDefs :: Typechecked [TypeDef]
getDefs = defs <$> ask

-- | Get the input type
getInput :: Typechecked Xtype
getInput = input <$> ask

-- | Get the content type
getContent :: Typechecked (Xtype)
getContent = content <$> ask

-- | Get the board size
getSize :: Typechecked (Int, Int)
getSize = size <$> ask

-- | Check whether (x,y) is in the bounds of the board
inBounds :: (Int, Int) -> Typechecked Bool
inBounds (x, y) = do
                    (x', y') <- getSize
                    return $ x <= x' && y <= y' && x > 0 && y > 0

-- | Extend the environment
localEnv :: ([(Name, Type)] -> [(Name, Type)]) -> Typechecked a -> Typechecked a
localEnv f e = local (\(Env a td b c d) -> Env (f a) td b c d) e

-- | Set the source line
setSrc :: (Expr SourcePos) -> Typechecked ()
setSrc e = modify (\(Stat _ x) -> Stat (Just e) x)

-- | Set the position
setPos :: (SourcePos) -> Typechecked ()
setPos e = modify (\stat -> stat{pos = e})

-- | Get the position
getPos :: Typechecked SourcePos
getPos = pos <$> get

-- | Get the source expression
getSrc :: Typechecked (Expr ())
getSrc = do
  e <- source <$> get
  case e of
    Nothing -> unknown "Unable to get source expression!"
    Just _e -> return $ clearAnn _e

-- | Get a type from the environment
getType :: Name -> Typechecked Type
getType n = do
  env <- getEnv
  inputT <- getInput
  contentT <- getContent
  case (lookup n env, lookup n (builtinT inputT contentT)) of
    (Just e, _) -> return e
    (_, Just e) -> return e
    _ -> notbound n

-- | Types for which the subtype relation is defined
--   In the Typechecked monad because subtyping depends on type definitions
class Subtypeable t where
   (<:) :: t -> t -> Typechecked Bool

instance Subtypeable Xtype where
   xa <: xb = do
               (xa', xb') <- derefs (xa, xb)
               return $ xa' <= xb'

instance Subtypeable Type where
   (Plain xa) <: (Plain xb) = xa <: xb
   (Function (Ft xa xb)) <: (Function (Ft xa' xb')) = do
                                                         inputs <- xa <: xa'
                                                         outputs <- xb <: xb'
                                                         return $ inputs && outputs
   _ <: _ = return False

-- | Attempt to unify two types
unify :: Xtype -> Xtype -> Typechecked Xtype
unify xa xb = do
                 (xa', xb') <- derefs (xa, xb)
                 unify' xa' xb'

-- | Attempt to unify two dereferenced types
unify' :: Xtype -> Xtype -> Typechecked Xtype
unify' (Tup xs) (Tup ys)
  | length xs == length ys = Tup <$> zipWithM unify' xs ys
unify' (X y z) (X w k)
  | y <= w = return $ X w (z `S.union` k) -- take the more defined type
  | w <= y = return $ X y (z `S.union` k)
unify' a b = mismatch (Plain a) (Plain b)

-- | Dereference a named type (to enable a subtype check)
deref :: Xtype -> Typechecked Xtype
deref (X (Named n) _) = do
                           ds <- getDefs
                           case find (\a -> fst a == n) ds of
                              Just (_, x) -> deref x
                              _           -> unknown "Internal type dereference error"
deref (Tup xs)        = do
                           xs' <- mapM deref xs
                           return $ Tup xs'
deref h               = return h

-- | Dereferences a type until it finds a tuple. If there is not tuple, returns the original type
--   This is used in 'eqntype' to ensure that params of multi-argument functions are assigned the
--   type of their respective tuple element in the signature.
--
--   TODO! write a test for this
findTuple :: Xtype -> Typechecked Xtype
findTuple x = do
            x' <- findTuple' x
            case x' of
               Tup xs -> return $ Tup xs
               _      -> return x

-- | Dereferences a type until it finds a tuple or no more dereferencing is possible
--   See 'findTuple' for more info
findTuple' :: Xtype -> Typechecked Xtype
findTuple' (X (Named n) _) = do
                           ds <- getDefs
                           case find (\a -> fst a == n) ds of
                              Just (_, Tup xs) -> return (Tup xs)
                              Just (_, x) -> findTuple' x
                              _           -> unknown "Internal type dereference error"
findTuple' (Tup xs)        = do
                           xs' <- mapM findTuple' xs
                           return $ Tup xs'
findTuple' h               = return h

-- | Dereference a pair of named types (a convenience function that wraps deref)
derefs :: (Xtype, Xtype) -> Typechecked (Xtype, Xtype)
derefs (xl, xr) = do
                     xl' <- deref xl
                     xr' <- deref xr
                     return (xl', xr')

-- | Check if t1 has type t2 with subsumption (i.e. by subtyping)
--   This is a wrapper around the Ord instance to produce the type error if there is a mismatch
hasType :: Xtype -> Xtype -> Typechecked Xtype
hasType (Tup xs) (Tup ys)
  | length xs == length ys = Tup <$> zipWithM hasType xs ys
hasType ta tb = ifM (ta <: tb) (return tb) (mismatch (Plain ta) (Plain tb))

-- | Returns a typechecked base type
t :: Btype -> Typechecked Xtype
t b = return (X b S.empty)

-- smart constructors for type errors

-- | Gets the source expression and its position from the 'Typechecked' monad
getInfo :: Typechecked (Expr (), SourcePos)
getInfo = ((,) <$> getSrc <*> getPos)

-- | Type mismatch error
mismatch :: Type -> Type -> Typechecked a
mismatch _t1 _t2 = getInfo >>= (\(e, x) -> throwError $ cterr (Mismatch _t1 _t2 e) x)

-- | Input type mismatch error
inputmismatch :: Type -> Typechecked a
inputmismatch act = do
                       (e, x) <- getInfo
                       it     <- getInput
                       throwError $ cterr (InputMismatch act (Plain it) e) x

-- | Type mismatch error for function application
appmismatch :: Name -> Type -> Type -> Typechecked a
appmismatch n _t1 _t2 = getInfo >>= (\(e, x) -> throwError $ cterr (AppMismatch n _t1 _t2 e) x)

-- | Not bound type error
notbound :: Name -> Typechecked a
notbound n  = getPos >>= \x -> throwError $ cterr (NotBound n) x

-- | Signature mismatch type error
sigmismatch :: Name -> Type -> Type -> Typechecked a
sigmismatch n _t1 _t2= getPos >>= \x -> throwError $ cterr (SigMismatch n _t1 _t2) x

-- | Signature mismatch type error
sigbadfeq :: Name -> Type -> Equation () -> Typechecked a
sigbadfeq n _t1 f = getPos >>= \x -> throwError $ cterr (SigBadFeq n _t1 f) x

-- | Unknown type error
unknown :: String -> Typechecked a
unknown s = getPos >>= \x -> throwError $ cterr (Unknown s) x

-- | Bad Op type error
badop :: Op -> Type -> Type -> Typechecked a
badop o _t1 _t2 = getInfo >>= (\(e, x) -> throwError $ cterr (BadOp o _t1 _t2 e) x)

-- | Out of Bounds type error
outofbounds :: Pos -> Pos -> Typechecked a
outofbounds _p sz = getPos >>= \x -> throwError $ cterr (OutOfBounds _p sz) x

-- | Uninitialized board type error
uninitialized :: Name -> Typechecked a
uninitialized n = getPos >>= \x -> throwError $ cterr (Uninitialized n) x

-- | Bad function application type error
badapp :: Name -> Expr SourcePos -> Typechecked a
badapp n e = getPos >>= \x -> throwError $ cterr (BadApp n (clearAnn e)) x

-- | Cannot dereference function type error
dereff :: Name -> Type -> Typechecked a
dereff n _t = getPos >>= \x -> throwError $ cterr (Dereff n _t) x

-- | Retrieve the extensions from an Xtype
extensions :: Xtype -> Typechecked (S.Set Name)
extensions (X _ xs) = return xs
extensions _        = unknown "No extension for type!" -- no extension for this
