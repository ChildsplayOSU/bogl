-- | Typechecker.

module Runtime.Typechecker (tc, tcexpr, environment) where

import Language.Syntax hiding (input, piece)
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Extra
import Debug.Trace
import Data.Either
import Data.Maybe
import Data.Bifunctor
import Data.List
import qualified Data.Set as S

type TypeEnv = [(Name, Type)]
data Env = Env {
  types :: TypeEnv,
  input :: Type,
  piece :: Type,
  size  :: (Int, Int)
               }
-- monadReader m =>
getEnv :: (Monad m) => ReaderT Env m TypeEnv
getEnv = types <$> ask

getInput :: (Monad m) => ReaderT Env m Type
getInput = input <$> ask

getPiece :: (Monad m) => ReaderT Env m Type
getPiece = piece <$> ask

getSize :: (Monad m) => ReaderT Env m (Int, Int)
getSize = size <$> ask

localEnv :: (Monad m) => ([(Name, Type)] -> [(Name, Type)]) -> ReaderT Env m a -> ReaderT Env m a
localEnv f e = local (\(Env a b c d) -> Env (f a) b c d) e

-- | Encoding the different type errors as types should let us do interesting things with them
data TypeError = Mismatch Type Type Expr     -- ^ Couldn't match two types in an expression
               | NotBound Name               -- ^ Name isn't (yet) bound in the enviroment
               | SigMismatch Name Type Type  -- ^ couldn't match the type of an equation with its signature
               | Unknown String              -- ^ Errors that "shouldn't happen"
               | BadOp Op Type Type Expr     -- ^ Can't perform a primitive operation
               | OutOfBounds (Int,Int) (Int,Int)

-- | smart constructors for type errors
mismatch :: Type -> Type -> Expr -> Typechecked a
mismatch t1 t2 e = throwError $ Mismatch t1 t2 e
notbound :: Name -> Typechecked a
notbound n = throwError $ NotBound n
sigmismatch :: Name -> Type -> Type -> Typechecked a
sigmismatch n t1 t2= throwError $ SigMismatch n t1 t2
unknown :: String -> Typechecked a
unknown s = throwError $ Unknown s
badop :: Op -> Type -> Type -> Expr -> Typechecked a
badop o t1 t2 e = throwError $ BadOp o t1 t2 e
outofbounds :: (Int, Int) -> (Int, Int) -> Typechecked a
outofbounds p sz = throwError $ OutOfBounds p sz

instance Show TypeError where
  show (Mismatch t1 t2 e) = "Could not match types " ++ show t1 ++ " and " ++ show t2 ++ "\n in expression: " ++ show e
  show (NotBound n) = "Variable " ++ n ++ " not bound in the enviroment!"
  show (SigMismatch n sig t) = "Signature for definition " ++ n ++ ": " ++ show sig ++ "\n does not match inferred type: " ++ show t
  show (Unknown s) = s
  show (BadOp o t1 t2 e) = "Cannot '" ++ show o ++ "' types " ++ show t1 ++ " and " ++ show t2 ++ "\n in expression: " ++ show e

-- | Things are typechecked with an enviroment ('ReaderT') and the possibility of failure ('ExceptT'). 
--   The typechecker is non-interactive so we do not need IO.
type Typechecked a =  (ReaderT Env (ExceptT TypeError Identity)) a

-- | Deconstruct a tuple type into a list of types. Partial function, probably should be removed DEPRECATED
detuple :: Type -> [Type]
detuple (Tup ts) = map Plain (map Pext ts)
detuple x = (trace $ show x) $ undefined

-- | Get the type of a valDef. Check the expression's type with the signature's. If they don't match, throw exception.
deftype :: ValDef -> Typechecked Type
deftype (Val (Sig n t) eqn) = do
  eqt <- eqntype t eqn
  if eqt == t
    then return t
    else sigmismatch n t eqt

deftype (BVal (Sig n t) eqn) = do
  eqt <- beqntype t eqn
  if eqt == t
    then return t
    else sigmismatch n t eqt

beqntype :: Type -> BoardEq -> Typechecked Type
beqntype t (PosDef _ xp yp e) = do 
   -- bounds checking on x and y positions?  
   t1 <- exprtype e     -- TODO: I think this needs to match the type of Board. Do I need to pass that in to the function or can I access it in some other way?
   b <- getPiece
   sz <- getSize
   case (t1 == b, sz >= (xp,yp)) of
     (True, True) -> return $ Plain $ Pext (X Board S.empty)
     (False, _) -> mismatch b t1
     (_, False) -> outofbounds (xp, yp) sz

-- | Get the type of an equation
eqntype :: Type -> Equation -> Typechecked Type
eqntype _ (Veq _ e) = exprtype e >>= (return . Plain)
eqntype (Function (Ft inputs _)) (Feq _ (Pars params) e) = do
    e' <- localEnv ((++) (zip params (i))) (exprtype e)
    return $ Function (Ft inputs (e'))
eqntype _ _ = throwError (Unknown "Environment corrupted.") -- this should never happen?

t :: Btype -> Typechecked Xtype
t b = (return . ext) b
-- Get the type of an expression
exprtype :: Expr -> Typechecked Xtype
exprtype (I _) = t Itype
exprtype (S "A") = return $ (X (Player) S.empty) -- TODO: manage this more elegantly?
exprtype (S "B") = return $ (X (Player) S.empty)
exprtype (S s) = t (Symbol s)
exprtype (B _) = t Booltype
exprtype (Let n e1 e2) = do
  t <- exprtype e1
  localEnv ((n, Plain t):) (exprtype e2)
exprtype (Ref s) = do
  x <- getType s
  case x of
    (Plain t) -> return t
    other -> unknown $ "Object " ++ s ++ " of type " ++ show other ++ " is a function and cannot be dereferenced."
exprtype (Tuple xs) = do
  xs' <- mapM exprtype xs
  return $ Tup xs'
exprtype e@(App n es) = do
  es' <- mapM exprtype es
  t <- getType n
  case t of
    (Function (Ft (Tup i) o)) -> if es' == i then return o else do
      ts <- mapM (xtype e) es'
      mismatch (Plain $ Tup ts) (Plain (Tup i)) e
    (Function (Ft i o)) -> if es' == [i] then return o else do
      ts <- mapM (xtype e) es'
      mismatch (Plain $ Tup ts) (Plain i) e
    _ -> do
      ts <- mapM (xtype e) es'
      mismatch (Function $ (Ft (Pt (Tup ts)) (Pext (X Undef S.empty)))) t e -- TODO Get expected output from enviroment (fill in Undef what we know it should be)

exprtype e@(Binop Equiv e1 e2) = do
  t1 <- exprtype e1
  t2 <- exprtype e2
  if (t1 == t2) then t Booltype else badop Equiv (Plain t1) (Plain t2) e
exprtype e@(Binop Get e1 e2) = do 
  t1 <- exprtype e1
  t2 <- exprtype e2
  if t1 == ext Board && t2 == ext Position
   then return $ wrap Position 
   else badop Get (Plain t1) (Plain t2) e -- TODO: bounds check?  can't at typechecking without dependent types, I think. might be worth looking into.
exprtype e@(Binop x e1 e2) = do
  t1 <- exprtype e1
  t2 <- exprtype e2
  case (t1, t2) of
    ((X Itype s1), (X Itype s2)) | S.null s1 && S.null s2 -> if x `elem` [Plus, Minus, Times, Div, Mod]
                                              then t Itype
                                              else if x `elem` [Less, Greater]
                                                   then t Booltype
                                                   else badop x (Plain t1) (Plain t2) e
    ((X Booltype s1), (X Booltype s2)) -> if x `elem` [And, Or, Xor] && S.null s1 && S.null s2
                                                    then p Booltype
                                                    else badop x (Plain t1) (Plain t2) e
    _ -> badop x (Plain t1) (Plain t2) e


-- if
exprtype e@(If e1 e2 e3) = do
  t1 <- exprtype e1
  t2 <- exprtype e2
  t3 <- exprtype e3
  case (t1, t2, t3) of
    ((X Booltype empty), (X (Symbol s) s1), (X t3' s2)) | S.null empty -> return $ (X t3' $ S.unions [s1, s2, S.singleton s])
    ((X Booltype empty), (X t2' s1), (X (Symbol s) s2)) | S.null empty -> return $ (X t2' $ S.unions [s1, s2, S.singleton s])
    ((X Booltype empty), y, z) | S.null empty -> if y /= z then mismatch (Plain y) (Plain z) e else return $ (y)
    (x, _, _) -> mismatch (Plain $ Pext $ (X Booltype S.empty)) (Plain x) e

-- case (FIXME)
exprtype expr@(Case n xs e) = do
  t1 <- getType n
  case t1 of
    Plain (X t' xs') | xs' /= S.empty -> if xs' == (S.fromList patterns) then compileCases n xs (t1, e) else unknown $ "Incomplete pattern match in "
                                                                                                   ++ show expr
                                                                                                   ++ " please match cases: "
                                                                                                   ++ (concat . S.toList) (xs' `S.difference` (S.fromList patterns))-- TODO: a better error
    _ -> unknown $ show t1 ++ " is not an extended type, so you can't pattern-match on it."
  where
    (patterns, exprs) = (map fst xs, map snd xs)
    compileCases :: Name -> [(Name, Expr)] -> (Type, Expr) -> Typechecked Xtype
    compileCases n xs e = do
      types <- mapM (fakeType n) (ts')
      (atom, extension) <- partitionM notSymbol types
      case atom of
        [(X x exten')] -> return $ (Pext (X x (exten' `S.union` (S.unions (map retrieveSymbols extension)))))
        h@((X x exten):xs) | all (== h) xs -> let exten' = (S.unions . map getExtensions)  (h:xs)
                                in return $ (Pext (X x (exten' `S.union` (S.unions (map retrieveSymbols extension)))))
        xs -> unknown $ "Cannot construct the type: " ++ ((intercalate "|") $ show <$> xs) ++ "\n produced by: " ++ (show expr)
      where
        ts' = e:(map (first singletonSymbol) xs)


    notSymbol ((X (Symbol _) _)) = return False
    notSymbol (X _) = return True
    notSymbol (_) = unknown "this is a function. I don't know what to do."
   
    fakeType :: Name -> (Type, Expr) -> Typechecked Xtype
    fakeType n (t, e) = localEnv ((n, atomicType t):) (exprtype e)

    retrieveSymbols (X (Symbol n) s) = (S.singleton n) `S.union` s
    retrieveSymbols _ = S.empty

exprtype (While c b n e) = exprtype e -- TODO 

getExtensions :: Xtype -> S.Set Name
getExtensions (X _ exs) = exs
getExtensions _ = S.empty

atomicType :: Type -> Type
atomicType (Plain (X t _)) = Plain (X t S.empty)

singletonSymbol :: Name -> Type
singletonSymbol n = Plain (Pext $ X (Symbol n) S.empty)


-- | Get the type of a reference in the enviroment
getType :: String -> Typechecked Type
getType n = do
  env <- getEnv
  case lookup n env of
    Just e -> return e
    Nothing -> notbound n

builtins = [
  ("input", Function (Ft (Pext (X Board S.empty)) (Pext (X Position S.empty)))),
  ("positions", Plain (Pext (X Positions S.empty))),
  ("place", Function (Ft (Pt (Tup [(X AnySymbol S.empty), (X Board S.empty), (X Position S.empty)]  )) (Pext (X Board S.empty)))),
  ("remove", Function (Ft (Pt (Tup [(X Board S.empty), (X Position S.empty)])) (Pext (X Board S.empty)))),
  ("inARow", Function (Ft (Pt (Tup [X Itype S.empty, X AnySymbol S.empty, X Board S.empty])) (Pext (X Booltype S.empty)))),
  ("isFull", Function (Ft (Pext (X Board S.empty)) (Pext (X Booltype S.empty)))),    
  ("at", Function (Ft (Pt (Tup [X Board S.empty, X Position S.empty])) (Pext (X AnySymbol S.empty))))
  -- This should be polymorphic over all types instead of over all symbols.
           ]


-- | Produce the environment
environment :: BoardDef -> InputDef -> [ValDef] -> Env
environment (BoardDef _ _ t) (InputDef i) vs = Env (map f vs ++ builtins) i t
  where f (Val (Sig n t1) eq) = (n, t1)
        f (BVal (Sig n t1) eq) = (n, t1)

-- | Run the typechecker on env and a list of ValDefs
runTypeCheck :: Env -> ValDef -> Either TypeError Type
runTypeCheck e v = runIdentity $ runExceptT $ runReaderT (deftype v) e

-- | Run the typechecker on a 'Game' and report any errors to the console.
tc :: Game -> IO Bool
tc (Game _ b i vs) = if all (isRight) (checked) then return True else ((putStrLn . (intercalate "\n\n\n") . (map show)) $ lefts checked) >> return False
  where
    env = environment b i vs
    checked = map (runTypeCheck env) vs

-- | Run the typechecker on an 'Expr' and report any errors to the console.
tcexpr :: Env -> Expr -> Either TypeError Xtype
tcexpr e x = runIdentity $ runExceptT $ runReaderT (exprtype x) e
