-- | Typechecker monad: TODO sort and document all these

module Typechecker.Monad where

import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader

import Language.Types hiding (input, piece, size)

import qualified Data.Set as S

import Language.Syntax hiding (piece, input, size)
import Runtime.Builtins

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
initEnv i p s = Env [] i p s
-- | Typechecker state
data Stat = Stat {
  holes :: TypeEnv,
  source :: Maybe Expr
            }

-- | Typechecking monad
type Typechecked a = (StateT Stat (ReaderT Env (ExceptT TypeError Identity))) a


-- | Run a computation inside of the typechecking monad
typecheck :: Env -> Typechecked a -> Either TypeError (a, Stat)
typecheck e a = runIdentity . runExceptT . (flip runReaderT e) $
                (runStateT a (Stat [] Nothing))
               
typeHoles e a = case typecheck e a of
  Left err -> Left err
  Right (x, stat) -> Right (x, holes stat)
-- | Add some types to the environment
extendEnv :: Env -> (Name, Type) -> Env
extendEnv (Env t i p s) v = Env (v:t) i p s

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


-- | Extend the environment
localEnv :: ([(Name, Type)] -> [(Name, Type)]) -> Typechecked a -> Typechecked a
localEnv f e = local (\(Env a b c d) -> Env (f a) b c d) e

-- | Get the current type holes
getHoles :: Typechecked TypeEnv
getHoles = holes <$> get

-- | Set the source line
setSrc :: Expr -> Typechecked ()
setSrc e = modify (\(Stat h _) -> Stat h (Just e))

-- | Get the source line
getSrc :: Typechecked Expr
getSrc = do
  e <- source <$> get
  case e of
    Nothing -> unknown "bad!" -- fixme
    Just e -> return e

-- | Get a type from the environment
getType :: Name -> Typechecked Type
getType n = do
  env <- getEnv
  case (lookup n env, lookup n builtinT) of
    (Just e, _) -> return e
    (_, Just e) -> return e
    _ -> notbound n

-- | add a type hole
addHole :: (Name, Type) -> Typechecked ()
addHole a = modify (\(Stat h s) -> Stat (a:h) s)

-- | Attempt to unify two types
unify :: Xtype -> Xtype -> Typechecked Xtype
unify (Tup xs) (Tup ys)
  | length xs == length ys = Tup <$> zipWithM unify xs ys
unify (Hole n) (Hole n2) = undefined
unify x (Hole n) = unify (Hole n) x
unify (Hole n) x = do
  hs <- getHoles
  case lookup n hs of
    Just (Plain t) -> if t <= x then return x else mismatch (Plain t) (Plain x) -- function holes FIXME
    Nothing -> addHole (n, Plain x) >> return x
unify (X y z) (X w k)
  | y <= w = return $ X w (z `S.union` k) -- take the more defined type
  | w <= y = return $ X y (z `S.union` k)
unify a b = mismatch (Plain a) (Plain b)


t :: Btype -> Typechecked Xtype
t b = return (X b S.empty)
-- | Encoding the different type errors as types should let us do interesting things with them
data TypeError = Mismatch Type Type Expr     -- ^ Couldn't match two types in an expression
               | NotBound Name               -- ^ Name isn't (yet) bound in the enviroment
               | SigMismatch Name Type Type  -- ^ couldn't match the type of an equation with its signature
               | Unknown String              -- ^ Errors that "shouldn't happen"
               | BadOp Op Type Type Expr     -- ^ Can't perform a primitive operation
               | OutOfBounds Pos Pos

-- | smart constructors for type errors
mismatch :: Type -> Type -> Typechecked a
mismatch t1 t2 = getSrc >>= (\e -> throwError $ Mismatch t1 t2 e)
notbound :: Name -> Typechecked a
notbound n  = throwError $ NotBound n
sigmismatch :: Name -> Type -> Type -> Typechecked a
sigmismatch n t1 t2= throwError $ SigMismatch n t1 t2
unknown :: String -> Typechecked a
unknown s = throwError $ Unknown s
badop :: Op -> Type -> Type -> Typechecked a
badop o t1 t2 = getSrc >>= (\e ->  throwError $ BadOp o t1 t2 e)
outofbounds :: Pos -> Pos -> Typechecked a
outofbounds p sz = throwError $ OutOfBounds p sz

-- | Retrieve the extensions from an Xtype
extensions :: Xtype -> Typechecked (S.Set Name)
extensions (X _ xs) = return xs
extensions a = throwError (Unknown $ "TYPE ERROR! CANT GET EXTENSIONS FROM " ++ show a)

instance Show TypeError where
  show (Mismatch t1 t2 e) = "Could not match types " ++ show t1 ++ " and " ++ show t2 ++ "\n in expression: " ++ show e
  show (NotBound n) = "You did not define " ++ n ++ " in this program"
  show (SigMismatch n sig t) = "Signature for definition " ++ n ++ ": " ++ show sig ++ "\n does not match inferred type: " ++ show t
  show (Unknown s) = s
  show (BadOp o t1 t2 e) = "Cannot '" ++ show o ++ "' types " ++ show t1 ++ " and " ++ show t2 ++ "\n in expression: " ++ show e
