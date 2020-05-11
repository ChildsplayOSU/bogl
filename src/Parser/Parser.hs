-- | Parser for BOGL

module Parser.Parser (parseLine, parseGameFile, expr, isLeft, parseAll, valdef, xtype, boardeqn, equation, Parser) where

import Parser.ParseError 
import Language.Syntax hiding (input, board)
import Language.Types
import Debug.Trace(trace)
import System.Directory

import Runtime.Builtins

import qualified Text.Parsec.Token as P

import Text.Parsec.Char 
import Text.Parsec.Language
import Text.Parsec.Expr
import Data.Maybe
import Control.Monad

import Text.Parsec
import qualified Data.Set as S

import Data.Either
import Data.List 

type SrcExpr = Expr Pos

-- | State for the parser
data ParState = PS {
  ctype :: Maybe Type,
  whileNames :: (Name, [Name]),
  ids :: [Name],
  syn :: [(Name, Xtype)]
                   }
-- | A parse context with builtins
startState = PS Nothing ("", []) (map fst builtins) []

type Parser = Parsec String (ParState)

-- | Get all used ids
getids :: Parser [Name]
getids = ids <$> getState

-- | Add a type synonym
addSyn :: (Name, Xtype) -> Parser ()
addSyn x = modifyState (\env -> env{syn = x:syn env})

-- | Lookup a type synonym
lookupSyn :: Name -> Parser Xtype
lookupSyn n = do
  t <- (lookup <$> (pure n) <*> (syn <$> getState))
  case t of
    Nothing -> fail $ "Type " ++ n ++ " not found!"
    Just t' -> return t'

-- | Add an id to list of used ids
addid :: Name -> Parser ()
addid n = do
  PS c w ids x <- getState
  putState (PS c w (n:ids) x)

-- | Get the names necessary to build 'While'
getWhileNames :: Parser (Name, [Name])
getWhileNames = whileNames <$> getState

-- | Put the names
putWhileNames :: (Name, [Name]) -> Parser ()
putWhileNames n = do
  PS c w ids x <- getState
  putState (PS c n ids x)

-- | Get the current type of the object being parsed (DEPRECATED due to new board syntax)
getCtype :: Parser (Maybe Type)
getCtype = ctype <$> getState

-- | Put the type of the object being parsed
putType :: Type -> Parser ()
putType t = do
  PS c w ids x <- getState
  putState (PS (Just t) w ids x)
-- | The 'Type' keywords
types = ["Bool", "Int", "AnySymbol", "Input", "Board"]

-- | The lexer, using the reserved keywords and operation names
lexer = P.makeTokenParser (haskellStyle {P.reservedNames = ["True", "False",
                                                            "let", "in", "if", "then", "else",
                                                            "while", "do", "game", "type", "Array", "of", "case", "type"
                                                            ] ++ types,
                                        P.reservedOpNames = ["=", "*", "==", "-", "/=", "/", "+", ":", "->", "{", "}"]})

-- | Operators (might want to fix the order of operations)
operators = [
             [op "!" (Binop Get) AssocLeft],
             [op "*" (Binop Times) AssocLeft, op "/" (Binop Div) AssocLeft, op "mod" (Binop Mod) AssocLeft],
             [op "+" (Binop Plus) AssocLeft, op "-" (Binop Minus) AssocLeft],
             [op "==" (Binop Equiv) AssocLeft]
            ]

-- | Parser for the 'Expr' datatype
expr :: Parser (Expr SourcePos)
expr = buildExpressionParser operators atom

-- | Helper function for handling operators
op s f assoc = Infix (reservedOp s *> pure f) assoc

lexeme = P.lexeme lexer
integer = P.integer lexer
int = fromInteger <$> P.integer lexer 
reserved = P.reserved lexer
parens = P.parens lexer
identifier = P.identifier lexer
whiteSpace = P.whiteSpace lexer 

-- | Ensure that the object parsed isn't already in state
new x = do
  ids' <- getids
  parsed <- x
  if parsed `elem` ids'
    then unexpected $ "redefinition of " ++ parsed
    else addid parsed >> return parsed

capIdentifier = lexeme ((:) <$> upper <*> (many alphaNum))
commaSep1 = P.commaSep1 lexer
commaSep = P.commaSep lexer
reservedOp = P.reservedOp lexer
charLiteral = P.charLiteral lexer
comma = P.comma lexer

atom :: Parser (Expr SourcePos)
atom =
  Annotation <$> getPosition <*> atom'

-- | Atomic expressions
atom' :: Parser (Expr SourcePos)
atom' =
  HE <$> ((char '?') *> identifier)
  <|>
  I <$> int
  <|>
  B <$> (reserved "True" *> pure True)
  <|>
  B <$> (reserved "False" *> pure False)
  <|>
  (try $ App <$> identifier <*> (parens (Tuple <$> (commaSep expr))))
  <|>
  S <$> capIdentifier
  <|>
  Ref <$> identifier
  <|>
  (try $ parens (expr <* notFollowedBy comma))
  <|>
  Tuple <$> parens (commaSep1 expr)
  <|>
  Let <$> (reserved "let" *> identifier) <*> (reservedOp "=" *> expr) <*> (reserved "in" *> expr)
  <|>
  If <$> (reserved "if" *> expr) <*> (reserved "then" *> expr) <*> (reserved "else" *> expr)
  <|>
  (do
      reserved "while"
      c <- atom
      reserved "do" 
      e <- atom 
      (recurse, names) <- getWhileNames
      let exprs = map Ref names
      let exprs' = case exprs of
            [x] -> x
            xs -> Tuple xs
      return $ While c e names exprs')
  <?> "expression"

params :: Name -> Parser [Name] 
params n = do 
   params <- parens $ commaSep1 identifier 
   let paramSet = nub params 
   if paramSet == params
      then return params 
      else 
         let repeats = params \\ paramSet in unexpected $ errRepeatParam repeats n     

-- | Equations
equation :: Parser (Equation SourcePos)
equation =
  (try $ (Veq <$> identifier <*> (reservedOp "=" *> expr)))
  <|>
  (try $ do
    name <- identifier
    params <- params name  
    putWhileNames (name, params)
    reservedOp "="
    e <- expr
    return $ Feq name (Pars params) e)

position :: Parser Pos
position =
   Index <$> int 
   <|>
   ForAll <$> identifier

-- | Board equations
boardeqn :: String -> Parser (BoardEq SourcePos)
boardeqn n = do 
   name <- string n <* (lexeme ((lexeme (char '!')) *> char '('))  
   xpos <- lexeme position <* lexeme comma 
   ypos <- lexeme position <* (lexeme (char ')') <* reservedOp "=") 
   case (xpos, ypos) of 
      (ForAll xn, ForAll yn) -> if xn == yn then unexpected (errRepeatParam [xn] name) 
         else do 
            exp <- expr
            return $ PosDef name xpos ypos exp  
      _ -> do 
            exp <- expr
            return $ PosDef name xpos ypos exp  

-- | Atomic types
btype :: Parser Btype
btype =
  reserved "Bool" *> pure Booltype
  <|>
  reserved "Int" *> pure Itype
  <|>
  reserved "Input" *> pure Input
  <|>
  reserved "Board" *> pure Board
  <|>
  reserved "AnySymbol" *> pure AnySymbol

enum :: Parser (S.Set Name)
enum = reservedOp "{" *> (S.fromList <$> (commaSep1 capIdentifier)) <* reservedOp "}"

-- | Extended types: types after the first are restricted to symbols
xtype :: Parser Xtype
xtype = (try $ do
  x1 <- xtype'
  reservedOp "&"
  enum' <- enum
  case x1 of
    (X b xs) -> return (X b (S.union xs enum'))
    a -> return a)
  <|>
      xtype'

xtype' :: Parser Xtype
xtype' =
  (try $ capIdentifier >>= lookupSyn)
  <|>
  (try $ X <$> (pure Top) <*> enum) -- ^ Plain enum
  <|>
  (try $ X <$> btype <*> (pure S.empty))
  <|>
  (try $ Tup <$> parens (lexeme ((:) <$> (xtype <* comma) <*> (commaSep1 xtype))))

-- |
--
-- >>> parseAll ttype "" "(Board, Int)" == Right (Tup [X Board S.empty, X Itype S.empty])
-- True
--
-- >>> parseAll ttype "" "(Symbol,Board)"
-- Right (Symbol,Board)
--
-- >>> isLeft $ parseAll ttype "" "(Symbol)"
-- True
--
-- >>> isLeft $ parseAll ttype "" "(3)"
-- True

-- | Function types
ftype :: Parser Ftype
ftype = do
  x <- xtype
  reservedOp "->"
  r <- xtype
  case x of
    Tup xs -> return $ Ft (Tup xs) r
    y -> return $ Ft (Tup [y]) r

-- | 'Type's
typ :: Parser Type
typ =
  (try $ (do
      f <- ftype
      putType (Function f)
      return (Function f)
  ))
  <|>
  (try $ (do
            p <- xtype
            putType (Plain p)
            return (Plain p)))

-- | Value signatures
sig :: Parser Signature
sig =
  Sig <$> new identifier <*> (reservedOp ":" *> typ)

-- | Value definitions
valdef :: Parser (ValDef SourcePos)
valdef = do
  (Sig n t) <- sig
  b <- getCtype
  case b of
    Just (Plain (X Board set)) | S.null set  -> (BVal (Sig n t)) <$> many1 (boardeqn n) <*> getPosition 
    _ -> (Val (Sig n t)) <$> (equation) <*> getPosition 

decl :: Parser (Maybe (ValDef SourcePos))
decl = (try $ (((,) <$> (reserved "type" *> new identifier) <*> (reservedOp "=" *> xtype)) >>= addSyn) >> return Nothing)
       <|> Just <$> valdef

-- | Board definition
board :: Parser BoardDef
board = do
  (reserved "type" *> reserved "Board" *> reservedOp "=") *> (reserved "Array" *> (lexeme . char) '(')
  x <- int 
  (lexeme . char) ',' 
  y <- int
  (lexeme . char) ')'
  boardType <- reserved "of" *> xtype
  guard (x > 0 && y > 0) <?> "board dimensions to be >= 1" 
  return $ BoardDef (x,y) boardType

-- | Input definition
input :: Parser InputDef
input =
  reserved "type" *> reserved "Input" *> reservedOp "=" *> (InputDef <$> xtype)

typesyn = (try $ (((,) <$> (reserved "type" *> new identifier) <*> (reservedOp "=" *> xtype)) >>= addSyn))

-- | Game definition
game :: [ValDef SourcePos] -> Parser (Game SourcePos)
game vs =
  Game <$> ((whiteSpace *> reserved "game") *> identifier) <*>
  (many typesyn *> board) <*> (many typesyn *> input) <*>
  ((\p -> vs ++ catMaybes p) <$> (many decl))

-- | Uses the parser p to parse all input, throws an error if anything is left over
parseAll :: Parser a -> String -> String -> Either ParseError a
parseAll p s = runParser (p <* eof) startState s

-- | Read from the file, and parse
parseFromFile p fname = do
  input <- readFile fname
  return (parseAll p fname input)
 
parseLine :: String -> Either ParseError (Expr SourcePos)
parseLine = parseAll expr ""

-- | Parse a game file, displaying an error if there's a problem (used in repl)
parseGameFile :: String -> IO (Either ParseError (Game SourcePos))
parseGameFile f = do
  exists <- doesFileExist "./Prelude.bglp"
  prel <- if exists then
             Parser.Parser.parseFromFile (many decl) "./Prelude.bglp" >>= \x -> case x of
              Right x -> return x
              Left err -> return [] -- FIXME. Need to warn the teacher that they wrote a corrupt Prelude.
            else return []
  parsed <- Parser.Parser.parseFromFile (game (catMaybes prel)) f
  return parsed
