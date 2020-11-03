{-|
Description : Data type for reporting type errors
-}

module Error.TypeError where

import Language.Syntax
import Language.Types
import Utils.String

-- | Categories of type errors; see show instance for more detail about the error
-- Note: any new constructors need to have a corresponding case in cterr (in Error.hs)
data TypeError = Mismatch      {t1 :: Type,  t2 :: Type, expr :: Expr ()}
               | AppMismatch   {name :: String, t1 :: Type,  t2 :: Type, expr :: Expr ()}
               | NotBound      {name :: String}
               | SigMismatch   {name :: String, sigType :: Type, actualType :: Type}
               | Unknown       {msg :: String}
               | BadOp         {op :: Op, t1 :: Type, t2 :: Type, expr :: Expr ()}
               | OutOfBounds   {xpos :: Pos, ypos :: Pos}
               | BadApp        {name :: String, arg :: Expr ()}
               | Dereff        {name :: String, typ :: Type}
               | Uninitialized {name :: String}
               | SigBadFeq     {name :: String, sigType :: Type, feq :: Equation ()}
               | InputMismatch {actual :: Type, expected :: Type, expr :: Expr()}
               deriving (Eq)

instance Show TypeError where
  show (Mismatch _t1 _t2 e)      = "Could not match types " ++ show _t1 ++ " and " ++ show _t2 ++ " in expression:\n\t" ++ show e
  show (AppMismatch n _t1 _t2 e) = "The function " ++ n ++ " requires type " ++ show _t1 ++ " but you provided type " ++ show _t2 ++ " in expression:\n\t" ++ show e
  show (NotBound n)              = "You did not define " ++ n
  show (SigMismatch n sig _t)    = "Signature for definition " ++ quote (n ++ " : " ++ show sig) ++ "\ndoes not match actual type " ++ show _t
  show (Unknown s)               = s
  show (BadOp o _t1 _t2 e)       = "Cannot '" ++ show o ++ "' types " ++ show _t1 ++ " and " ++ show _t2 ++ " in expression:\n\t" ++ show e
  show (OutOfBounds x y)         = "Could not access (" ++ show x ++ "," ++ show y ++ ") on the board, this is not a valid space. "
  show (BadApp n e)              = "Could not apply " ++ n ++ " to " ++ show e ++ "; it is not a function."
  show (Dereff n _t)             = "Could not dereference the function " ++ n ++ " with type " ++ show _t ++ ". Maybe you forgot to give it arguments."
  show (Uninitialized n)         = "Incomplete initialization of Board " ++ quote n
  show (SigBadFeq n sig f)       = quote (n ++ " : " ++ show sig) ++ " cannot be defined with the function equation\n\t" ++ show f
  show (InputMismatch act xp e)  = "Got type " ++ show act ++ " but expected type " ++ show xp ++ " from input:\n\t" ++ show e
