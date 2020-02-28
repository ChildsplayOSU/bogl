-- | Evaluation Monad

module Runtime.Monad where

import Runtime.Values
import Language.Syntax

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity


type Eval a = StateT Buffer (ExceptT Exception (ReaderT Env (Identity))) a

-- | Call-by-value semantics
data Env = Env {
  evalEnv :: EvalEnv  ,
  boardSize :: (Int, Int)
               }
  deriving Show

getBounds :: Eval (Int, Int)
getBounds = boardSize <$> ask

getEnv :: Eval (EvalEnv)
getEnv = evalEnv <$> ask

emptyEnv x = Env [] x

modifyEval :: (EvalEnv -> EvalEnv) -> Env -> Env
modifyEval f (Env e b) = Env (f e) b

-- | Input buffer
type Buffer = [Val]
-- | Exceptions
data Exception =
  NeedInput Val | -- ^ Ran out of input, and here's the current board
  Error String -- ^ Encountered a runtime error (shouldn't ever happen)
  deriving (Eq, Show)

-- | Evaluation occurs in the Identity monad with these side effects:
-- ReaderT: Evaluation enviroment, board size and piece type, and input type
-- StateT: Input buffer, used for reading input

-- | Evaluate in the environment given, with a buffer.
runEval :: Env -> Buffer -> Eval a -> Either Exception a
runEval env buf x = runIdentity (runReaderT (runExceptT (evalStateT x buf)) env)

-- | Evaluate with an extended scope
extScope :: EvalEnv -> Eval a -> Eval a
extScope env = local (modifyEval (env++))

-- | Lookup a name in the environment FIXME
lookupName :: Name -> Eval (Maybe Val)
lookupName n = do
  env <- (evalEnv <$> ask)
  case lookup n env of
    Just v -> (return . Just) v
    Nothing -> return Nothing

-- | Ask for input, displaying a value to the user
waitForInput :: Val -> Eval a
waitForInput v = throwError (NeedInput v)

err :: String -> Eval a
err n = throwError (Error n)

-- | Read input
readTape :: Val -> Eval (Val)
readTape v = do
  tape <- get
  case tape of
    (x:xs) -> (put xs) >> return x
    [] -> waitForInput v

-- | Helper function to get the Bool out of a value. This is a partial function.
unpackBool :: Val -> Bool
unpackBool (Vb b) = b
unpackBool _ = undefined


-- | Bind the value of a definition to its name in the current Environment
