{-|
Module      : Runtime.Eval
Description : Interpreter for Spiel
Copyright   : (c)
License     : BSD-3
-}

module Runtime.Eval where
import Language.Syntax

import Runtime.Values
import Runtime.Monad
import Runtime.Builtins
import Error.RuntimeError

import Control.Monad
import Data.Array

import Control.Monad.Writer
import Control.Monad.State

import Text.Parsec.Pos
import qualified Data.Map.Strict as Map()


-- | Produce all of the bindings from a list of value definitions.
--   This is done sequentially. Errors are reported as they're found.
bindings :: (Int, Int) -> [ValDef a] -> Writer [Exception] Env
bindings sz vs = e
  where
    e = foldM (\env (n, v) -> case runEval env ([], [], 1) v of
                                Right v' -> return $ modifyEval (insertEvalEnv (n, v')) env
                                Left _err -> (tell [_err]) >> return env)
        (emptyEnv sz)
        (map bind vs)

-- | Partial application of of args to 'bindings', allows passing in just a list of ValDefs
bindings_ :: (Int, Int) -> [ValDef a] -> Env
bindings_ x y = (fst . runWriter) (bindings x y)

-- | Binds a ValDef to a name and a Val in the Eval monad
bind :: (ValDef a) -> (Name, Eval Val)
-- | Binds a value equation
bind (Val _ (Veq n e) _) = do
  (n, do
    env <- getEnv
    -- bind as a Pending Value
    -- this allows lazy evaluation of vals that use input at runtime
    return $ Pv env (clearAnn e))
-- | Binds a function equation
bind  (Val _ (Feq n (Pars ls) e) _) = (n, do
                                        env <- getEnv
                                        return $ Vf ls env (clearAnn e))
-- | Binds a board equation
bind (BVal (Sig n _) defs _) = (n, do
      sz <- getBounds
      values <- mapM (eval . boardExpr) defs
      maybeBoard <- lookupName n
      case maybeBoard of
         -- no board in the envs
         Nothing         -> return $ Vboard (fill (newBoard sz) sz defs values)
         -- existing board in the env
         Just (Vboard b) -> return $ Vboard (fill b sz defs values)
         -- lookup error
         _               -> throwRuntimeError (InvalidLookup n))
     where
        newBoard sz = array ((1,1), sz)
                      (zip [(x,y) | x <- [1..(fst sz)], y <- [1..(snd sz)]] (repeat (Vs "?")))
        fill _board sz ds vs = foldl (\b p -> updateBoard b sz (fst p) (snd p)) _board (zip ds vs)

-- | Updates a board
updateBoard :: Board -> (Int, Int) -> (BoardEq a) -> Val -> Board
updateBoard b sz d v = let _indices = range ((1,1), sz) in
                              b // zip (filter (posMatches (xpos d) (ypos d)) _indices) (repeat v)

-- | Check if a Pos matches a coordinate pair
posMatches :: Pos -> Pos -> (Int, Int) -> Bool
posMatches xp yp (x, y) = match xp x && match yp y
   where
      match (ForAll _) _        = True
      match (Index ix) i = ix == i

-- | Attempt to access a position on the board that may be invalid
-- If the position is invalid, return an Err value instead of causing an actual array access error
tryUnsafeBoardAccess :: (Int,Int) -> Board -> Eval Val
tryUnsafeBoardAccess (x,y) arr = let (_,(bx,by)) = bounds arr in
   case (x < 1 || x > bx || y < 1 || y > by) of
     False -> return $ arr ! (x,y) -- good index
     True  -> throwRuntimeError $ InvalidBoardAccess (x,y) (bx,by)

-- | Binary operation evaluation
evalBinOp :: Op -> (Expr a) -> (Expr a) -> Eval Val
evalBinOp Plus l r     = evalNumOp "+" (+) l r
evalBinOp Minus l r    = evalNumOp "-" (-) l r
evalBinOp Times l r    = evalNumOp "*" (*) l r
evalBinOp Div l r      = evalNumOp "/" div l r
evalBinOp Mod l r      = evalNumOp "%" mod l r
evalBinOp Less l r     = evalCompareOpInt (<) l r
evalBinOp Leq l r      = evalCompareOpInt (<=) l r
evalBinOp Equiv l r    = evalEq (==) l r
evalBinOp NotEquiv l r = evalEq (/=) l r
evalBinOp Geq l r      = evalCompareOpInt (>=) l r
evalBinOp Greater l r  = evalCompareOpInt (>) l r
evalBinOp Get l r      = do
   _board <- eval l
   pos    <- eval r
   case (_board, pos) of
      (Vboard arr, Vt [Vi x, Vi y]) -> tryUnsafeBoardAccess (x,y) arr
      _                             -> throwRuntimeError (UnexpectedEvaluation $ "Could not access " ++ show r ++ " on the board \n" ++ show l)

-- | evaluates the == and /= operations
evalEq :: (Val -> Val -> Bool) -> (Expr a) -> (Expr a) -> Eval Val
evalEq f l r = do
                  v1 <- eval l
                  v2 <- eval r
                  return $ Vb (f v1 v2)

-- | evaluates comparison operations for only Ints (except for == & /=)
evalCompareOpInt :: (Int -> Int -> Bool) -> (Expr a) -> (Expr a) -> Eval Val
evalCompareOpInt f l r = do
                     v1 <- eval l
                     v2 <- eval r
                     case (v1, v2) of
                        (Vi l', Vi r')  -> return (Vb (f l' r'))
                        _               -> throwRuntimeError $ BadComparison (show l) (show r)

-- | evaluates numerical operations
evalNumOp :: String -> (Int -> Int -> Int) -> (Expr a) -> (Expr a) -> Eval Val
evalNumOp sym f l r =
   do
        v1 <- eval l
        v2 <- eval r
        case (v1, v2) of
          -- if div/mod and denominator is 0, report an error, otherwise proceed
          (Vi l', Vi r') -> if r' == 0 && (sym == "/" || sym == "%")
                              then throwRuntimeError DivideByZero -- err "Cannot divide by zero" --return (Err "Cannot divide by zero") --
                              else return (Vi (f l' r'))
          _              -> throwRuntimeError $ BadNumericalOp (show l) (show r)

-- | Evaluates an expression at runtime, producing a Val in the Eval monad
eval :: (Expr a) -> Eval Val
-- evaluate an Annotation
eval (Annotation _ e) = eval e
-- evaluate an Integer
eval (I i) = return $ Vi i
-- evaluate a Boolean
eval (B b) = return $ Vb b
-- evaluate a Symbol
eval (S s) = return $ Vs s
-- evaluate a Tuple
eval (Tuple es) = mapM eval es >>= (return . Vt)
-- evaluate a Ref
eval (Ref n) = do
  -- verify builtinRefs first, otherwise we risk forcibly evaluating the entire env
  -- for a non-existant ref (the case for 'input')
  let b = lookup n builtinRefs
  case b of
    (Just v) -> v -- builtin to reference
    _        -> do
                -- possible non builtin ref to lookup
                e <- lookupName n
                case e of
                  -- valid reference
                  (Just v) -> case v of
                    -- Pending Value, need to eval this to get the actual value
                    (Pv env e') -> extScope env $ evalWithLimit $ eval e'
                    -- normal value, return as is
                    _         -> return $ v
                  --  undefined reference
                  _ -> throwRuntimeError (UndefinedReference n)

-- evalute a function application
eval (App n es) = do
  args <- eval es >>= \x -> case x of
    (Vt args)      -> return args
    _              -> return [x]
  -- previously, 'lookupName' was hit first, and this caused the entire env
  -- to be evaluated before the builtins were checked. Verifying the builtins first,
  -- which are finite, prevents this
  case lookup n builtins of
    Just f2 -> do
      f2 args
    Nothing -> do
      f <- lookupName n
      case f of
        Just (Vf params env' e) -> extScope (extendEvalEnv (zip params args) env') (evalWithLimit (eval e))
        Just (Pv env' e)        -> extScope env' (evalWithLimit (eval e))
        Nothing                 -> throwRuntimeError (UndefinedReference n)
        _                       -> throwRuntimeError (InvalidLookup n)

-- evaluate a Let expression
eval (Let n e1 e2) = do
  v <- eval e1
  extScope (insertEvalEnv (n,v) emptyEvalEnv) (eval e2)

-- evaluate an If-Then-Else expression
eval (If p e1 e2) = do
  mb <- eval p
  case mb of
    (Vb bb)      -> if bb then evalWithLimit $ eval e1 else evalWithLimit $ eval e2
    _            -> throwRuntimeError (UnexpectedEvaluation e)
      where e = "The expression " ++ show p ++ " did not evaluate to a Bool as expected!"

-- evaluate a BinOp expression
eval (Binop op e1 e2) = evalBinOp op e1 e2

-- evaluate a while expression
eval (While c b names exprs) = do
   c' <- eval c   -- evaluate the condition
   case c' of
      (Vb True)  -> do
         env <- getEnv         -- get the current environment
         result <- eval b      -- evaluate the body
         case result of        -- update the variables in the environment w/ new values and recurse:
            (Vt vs) -> extScope (extendEvalEnv (zip names vs) env) recurse
            r       -> extScope (insertEvalEnv (head names, r) env) recurse -- that head should never fail...
      (Vb False) -> do
        e <- eval exprs
        return e
      er2@(Err _)  -> return er2  -- propagate any error produced by the condition
      _            -> throwRuntimeError (UnexpectedEvaluation $ "The expression " ++ show c ++
                                     " did not evaluate to a Bool as expected!")
   where
      recurse = evalWithLimit $ eval (While c b names exprs)

-- evaluate a type hole
-- TODO, we don't have type holes in BoGL anymore, phase this out
eval (HE _) = throwRuntimeError (UnexpectedEvaluation "Type hole: ") -- type hole...

-- | Runs an expression under a given environment and a given buffer, producing
-- a result of either a list of values and a buffer, or a list of values and a single value
runWithBuffer :: Env -> Buffer -> (Expr SourcePos) -> Either ([Val], Buffer) ([Val], Val)
runWithBuffer env buf e = do
  let v = runEval env buf (eval' e) in
    case v of
      Left (NeedInput bs) -> Left (bs, buf)
      Right (boards, val) -> Right (boards, val)
      Left (Error er)     -> Right $ ([], Err er) -- not good
   where
      eval' :: (Expr a) -> Eval ([Val], Val)
      eval' expr = do
         v <- (eval expr)
         (_, boards, _) <- get
         return (boards, v)
