{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-} -- why isn't this on by default :(

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
import Data.Aeson
import Control.Monad.Reader
import Text.Parsec.Pos


import Language.Types hiding (piece, size)

import qualified Data.Set as S

import Language.Syntax hiding (input)
import Runtime.Builtins

import Utils.String

-- | Types in the environment
type TypeEnv = [(Name, Type)]

-- | Typechecker environment
data Env = Env {
  types :: TypeEnv,
  input :: Xtype,
  piece :: Xtype,
  size  :: (Int, Int)
               }

-- | Initial empty environment
initEnv :: Xtype -> Xtype -> (Int, Int) -> Env
initEnv i _p s = Env [] i _p s

-- | An example environment for interal use (e.g. testing, ghci)
exampleEnv :: Env
exampleEnv = Env (builtinT intxt intxt) intxt intxt (5, 5)

-- | Typechecker state
data Stat = Stat {
  holes :: TypeEnv,
  source :: Maybe (Expr SourcePos),
  pos :: SourcePos
            }

-- | Typechecking monad
type Typechecked a = (StateT Stat (ReaderT Env (ExceptT TypeError Identity))) a


-- | Run a computation inside of the typechecking monad
typecheck :: Env -> Typechecked a -> Either TypeError (a, Stat)
typecheck e a = runIdentity . runExceptT . (flip runReaderT e) $
                (runStateT a (Stat [] Nothing (newPos "" 0 0)))

-- | Typecheck type holes
typeHoles :: Env -> Typechecked a -> Either TypeError (a, TypeEnv)
typeHoles e a = case typecheck e a of
  Left err -> Left err
  Right (x, stat) -> Right (x, holes stat)

-- | Add some types to the environment
extendEnv :: Env -> (Name, Type) -> Env
extendEnv (Env _t i _p s) v = Env (v:_t) i _p s

-- | Get the type environment
getEnv :: Typechecked TypeEnv
getEnv = types <$> ask

-- | Get the input type
getInput :: Typechecked Xtype
getInput = input <$> ask

-- | Get the piece type
getPiece :: Typechecked (Xtype)
getPiece = piece <$> ask

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
localEnv f e = local (\(Env a b c d) -> Env (f a) b c d) e

-- | Get the current type holes
getHoles :: Typechecked TypeEnv
getHoles = holes <$> get

-- | Set the source line
setSrc :: (Expr SourcePos) -> Typechecked ()
setSrc e = modify (\(Stat h _ x) -> Stat h (Just e) x)

-- | Set the position
setPos :: (SourcePos) -> Typechecked ()
setPos e = modify (\stat -> stat{pos = e})

-- | Get the position
getPos :: Typechecked SourcePos
getPos = pos <$> get

-- | Get the source line
getSrc :: Typechecked (Expr SourcePos)
getSrc = do
  e <- source <$> get
  case e of
    Nothing -> unknown "!" -- TODO FIXME: This seems to popup when you assign an invalid type on a board (I think),
                           -- either that or an invalid index, one of the 2 (can be checked) @montymxb
    Just _e -> return _e

-- | Get a type from the environment
getType :: Name -> Typechecked Type
getType n = do
  env <- getEnv
  inputT <- getInput
  pieceT <- getPiece
  case (lookup n env, lookup n (builtinT inputT pieceT)) of
    (Just e, _) -> return e
    (_, Just e) -> return e
    _ -> notbound n

-- | add a type hole
addHole :: (Name, Type) -> Typechecked ()
addHole a = modify (\(Stat h s e) -> Stat (a:h) s e)

-- | Attempt to unify two types
unify :: Xtype -> Xtype -> Typechecked Xtype
unify (Tup xs) (Tup ys)
  | length xs == length ys = Tup <$> zipWithM unify xs ys
unify (Hole _) (Hole _) = undefined
unify x (Hole n) = unify (Hole n) x
unify (Hole n) x = do
  hs <- getHoles
  case lookup n hs of
    Just (Plain _t) -> if _t <= x then return x else mismatch (Plain _t) (Plain x) -- function holes FIXME
    Nothing -> addHole (n, Plain x) >> return x
    _       -> undefined -- unhandled case when a lookup does not match one of the above
unify (X y z) (X w k)
  | y <= w = return $ X w (z `S.union` k) -- take the more defined type
  | w <= y = return $ X y (z `S.union` k)
unify a b = mismatch (Plain a) (Plain b)

-- | Returns a typechecked base type
t :: Btype -> Typechecked Xtype
t b = return (X b S.empty)

-- | Encoding the different type errors as types should let us do interesting things with them
data TypeError = Mismatch {t1 :: Type,  t2 :: Type, srcPos2 :: (Expr SourcePos), srcPos :: SourcePos}                   -- ^ Couldn't match two types in an expression
               | AppMismatch {name :: Name, t1 :: Type,  t2 :: Type, srcPos2 :: (Expr SourcePos), srcPos :: SourcePos}  -- ^ Couldn't match function parameter and argument type
               | NotBound {name :: Name, srcPos :: SourcePos}                                                           -- ^ Name isn't (yet) bound in the enviroment
               | SigMismatch {name :: Name, sigType :: Type, actualType :: Type, srcPos :: SourcePos}                   -- ^ couldn't match the type of an equation with its signature
               | Unknown {msg :: String, srcPos :: SourcePos}                                                           -- ^ Errors that "shouldn't happen"
               | BadOp {op :: Op, t1 ::Type, t2 :: Type, srcPos2 :: (Expr SourcePos), srcPos :: SourcePos}              -- ^ Can't perform a primitive operation
               | OutOfBounds {xpos :: Pos, ypos :: Pos, srcPos :: SourcePos}
               | BadApp {name :: Name, arg :: (Expr SourcePos), srcPos :: SourcePos}                                    -- ^ An attempt to apply a non-function expr as if it were a function
               | Dereff {name :: Name, typ :: Type, srcPos :: SourcePos}                                                -- ^ An attempt to dereference a function
               | Uninitialized {name :: Name, srcPos :: SourcePos}
               deriving (Eq)

instance ToJSON TypeError where
  toJSON te = let src = srcPos te in object ["message" .= (show te), "line" .= sourceLine src, "col" .= sourceColumn src]

-- smart constructors for type errors

-- | Type mismatch error
mismatch :: Type -> Type -> Typechecked a
mismatch _t1 _t2 = ((,) <$> getSrc <*> getPos) >>= (\(e, x) -> throwError $ Mismatch _t1 _t2 e x)

-- | Type mismatch error for function application
appmismatch :: Name -> Type -> Type -> Typechecked a
appmismatch n _t1 _t2 = ((,) <$> getSrc <*> getPos) >>= (\(e, x) -> throwError $ AppMismatch n _t1 _t2 e x)

-- | Not bound type error
notbound :: Name -> Typechecked a
notbound n  = getPos >>= \_p -> throwError $ NotBound n _p

-- | Signature mismatch type error
sigmismatch :: Name -> Type -> Type -> Typechecked a
sigmismatch n _t1 _t2= getPos >>= \_p -> throwError $ SigMismatch n _t1 _t2 _p

-- | Unknown type error
unknown :: String -> Typechecked a
unknown s = getPos >>= (\x -> throwError $ Unknown s x)

-- | Bad Op type error
badop :: Op -> Type -> Type -> Typechecked a
badop o _t1 _t2 = ((,) <$> getSrc <*> getPos) >>= (\(e, x) ->  throwError $ BadOp o _t1 _t2 e x)

-- | Out of Bounds type error
outofbounds :: Pos -> Pos -> Typechecked a
outofbounds _p sz = getPos >>= \x -> throwError $ OutOfBounds _p sz x

-- | Uninitialized board type error
uninitialized :: Name -> Typechecked a
uninitialized n = getPos >>= \x -> throwError $ Uninitialized n x

-- | Bad function application type error
badapp :: Name -> Expr SourcePos -> Typechecked a
badapp n e = getPos >>= \_p -> throwError $ BadApp n e _p

-- | Cannot dereference function type error
dereff :: Name -> Type -> Typechecked a
dereff n _t = getPos >>= \_p -> throwError $ Dereff n _t _p

-- | Retrieve the extensions from an Xtype
extensions :: Xtype -> Typechecked (S.Set Name)
extensions (X _ xs) = return xs
extensions _        = unknown "No extension for type!" -- no extension for this

-- | Produce a human readable error string from a source position
errString :: SourcePos -> String
errString _p = case sourceName _p of
               "" -> str ++ " in the Interpreter expression" ++ "\n"
               _  -> str ++ "\n"
            where str = "Type error at: " ++ show _p

instance Show TypeError where
  show (Mismatch _t1 _t2 e _p)      = errString _p ++ "Could not match types " ++ show _t1 ++ " and " ++ show _t2 ++ " in expression:\n\t" ++ show e
  show (AppMismatch n _t1 _t2 e _p) = errString _p ++ "The function " ++ n ++ " requires type " ++ show _t1 ++ " but you provided type " ++ show _t2 ++ " in expression:\n\t" ++ show e
  show (NotBound n _p)              = errString _p ++ "You did not define " ++ n
  show (SigMismatch n sig _t _p)    = errString _p ++ "Signature for definition " ++ quote (n ++ " : " ++ show sig) ++ "\ndoes not match actual type " ++ show _t
  show (Unknown s _p)               = errString _p ++ s
  show (BadOp o _t1 _t2 e _p)       = errString _p ++ "Cannot '" ++ show o ++ "' types " ++ show _t1 ++ " and " ++ show _t2 ++ " in expression:\n\t" ++ show e
  show (OutOfBounds x y _p)         = errString _p ++ "Could not access (" ++ show x ++ "," ++ show y ++ ") on the board, this is not a valid space. "
  show (BadApp n e _p)              = errString _p ++ "Could not apply " ++ n ++ " to " ++ show e ++ "; it is not a function."
  show (Dereff n _t _p)             = errString _p ++ "Could not dereference the function " ++ n ++ " with type " ++ show _t ++ ". Maybe you forgot to give it arguments."
  show (Uninitialized n _p)         = errString _p ++ "Incomplete initialization of Board " ++ quote n -- Shows an incomplete board initialization
