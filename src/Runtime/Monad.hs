-- | Evaluation Monad

module Runtime.Monad where

import Runtime.Values
import Language.Syntax


import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import qualified Data.Map.Strict as Map


-- | Eval Monad transformer
type Eval a = StateT Buffer (ExceptT Exception (ReaderT Env (Identity))) a

-- | Call-by-value semantics
data Env = Env {
  evalEnv :: EvalEnv  ,
  boardSize :: (Int, Int)
               }
  deriving Show

-- | Uses the StateT monad to request the typed board dimensions in the runtime environment
getBounds :: Eval (Int, Int)
getBounds = boardSize <$> ask

-- | Uses the StateT monad to get the evaluation environment in the runtime environment
getEnv :: Eval (EvalEnv)
getEnv = evalEnv <$> ask

-- | Produces an empty environment for testing, and for starting evaluations
emptyEnv :: (Int,Int) -> Env
emptyEnv x = Env Map.empty x

-- | Modifies the evaluation environment, producing a new environment
modifyEval :: (EvalEnv -> EvalEnv) -> Env -> Env
modifyEval f (Env e b) = Env (f e) b

-- | Input buffer and display buffer.
--   The display buffer stores all boards which are to be printed on the front end after
--   a board is updated
--   Additionally counts the number of evaluation iterations,
--   stopping after a fixed amount with a 'Error $ "Stack Overflow!"'
type Buffer = ([Val], [Val], Int)

-- | Exceptions
data Exception =
  NeedInput [Val] | -- ^ Ran out of input and here's the buffered display boards
  Error String -- ^ Encountered a runtime error
  deriving (Eq, Show)


-- | Take an expressions, and before evaluating it checks & updates evaluation iterations
-- If the count is less than the limit, continue evaluating,
-- otherwise return an error instead of evluating further.
-- Prevents infinite loops via recursion, while, or
-- self referencing value equations, among other things
evalWithLimit :: Eval Val -> Eval Val
evalWithLimit e = do
  (tape,bord,iters) <- get
  put (tape,bord,iters+1)
  case iters < 5000 of -- hard limit of 5k iterations before stopping
    True  -> e
    False -> return $ Err $ "Your expression took too long to evaluate and was stopped! "
                            ++ "Please double check your program and try again."

-- | Evaluation occurs in the Identity monad with these side effects:
-- ReaderT: Evaluation enviroment, board size and content type, and input type
-- StateT: Input buffer, used for reading input

-- | Evaluate in the environment given, with a buffer.
runEval :: Env -> Buffer -> Eval a -> Either Exception a
runEval env buf x = runIdentity (runReaderT (runExceptT (evalStateT x buf)) env)

-- | Evaluate with an extended scope
extScope :: EvalEnv -> Eval a -> Eval a
extScope env = local (modifyEval (Map.union env))

-- | Lookup a name in the environment FIXME
lookupName :: Name -> Eval (Maybe Val)
lookupName n = do
  env <- getEnv
  case Map.lookup n env of
    Just v -> (return . Just) v
    Nothing -> return Nothing

-- | Ask for input, displaying a value to the user
waitForInput :: [Val] -> Eval a
waitForInput vs = throwError (NeedInput vs)

-- | Converts a string into an evaluation error
err :: String -> Eval a
err n = throwError (Error n)

-- | Read input
readTape :: Eval Val
readTape = do
  (tape, boards, iters) <- get
  case tape of
    (x:xs) -> (put (xs, boards, iters)) >> return x
    [] -> waitForInput boards

-- | Helper function to get the Bool out of a value. This is a partial function.
unpackBool :: Val -> Maybe Bool
unpackBool (Vb b) = Just b  -- Just a boolean
unpackBool _      = Nothing -- Not a valid boolean, should trip a runtime error
