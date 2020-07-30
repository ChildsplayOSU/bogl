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

-- | Input buffer and display buffer.
--   The display buffer stores all boards which are to be printed on the front end after an
--   expression is evaluated.
type Buffer = ([Val], [Val])

-- | Exceptions
data Exception =
  NeedInput [Val] | -- ^ Ran out of input and here's the buffered display boards
  Error String -- ^ Encountered a runtime error
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
waitForInput :: [Val] -> Eval a
waitForInput vs = throwError (NeedInput vs)

err :: String -> Eval a
err n = throwError (Error n)

-- | Read input
readTape :: Eval Val
readTape = do
  (tape, boards) <- get
  case tape of
    (x:xs) -> (put (xs, boards)) >> return x
    [] -> waitForInput boards

-- | Helper function to get the Bool out of a value. This is a partial function.
unpackBool :: Val -> Maybe Bool
unpackBool (Vb b) = Just b
unpackBool v = Nothing      -- not a valid boolean, should trip a runtime error


-- | Bind the value of a definition to its name in the current Environment
