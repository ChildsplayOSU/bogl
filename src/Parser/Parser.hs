-- | Parser for BOGL

module Parser.Parser (parseLine, parseGameFile, expr, isLeft, parseAll, valdef, xtype, Parser) where

import Language.Syntax hiding (input, board)
import Language.Types
import Debug.Trace(trace)

import qualified Text.Parsec.Token as P

import Text.Parsec.Language
import Text.Parsec.Expr
import Data.Maybe
import Control.Monad

import Text.Parsec
import qualified Data.Set as S

import Data.Either

-- | State for the parser
data ParState = PS {
  ctype :: Maybe Type,
  whileNames :: (Name, [Name]),
  ids :: [Name],
  syn :: [(Name, Xtype)]
                   }
-- | An empty parse context
emptyState = PS Nothing ("", []) [] []

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
    Nothing -> fail "Type not found!"
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
types = ["Bool", "Int", "AnySymbol", "Input", "Board", "Position", "Positions"]
-- | The lexer, using the reserved keywords and operation names
lexer = P.makeTokenParser (haskellStyle {P.reservedNames = ["if", "then", "True", "False",
                                                            "let", "in", "if", "then", "else",
                                                            "while", "do", "game", "type", "Grid", "of", "case", "type"
                                                            --"place", "remove", "isFull", "inARow", "at"
                                                            -- "A", "B", "free", "place", "next", "isFull", "inARow",
                                                            -- "countBoard", "countColumn", "countRow" ]
                                                            ] ++ types,
                                        P.reservedOpNames = ["=", "*", "==", "-", "/=", "/", "+", ":", "->", "<", "&", "{", "}"]})



-- | Operators (might want to fix the order of operations)
operators = [
             [op "!" (Binop Get) AssocLeft],
             [op "*" (Binop Times) AssocLeft, op "/" (Binop Div) AssocLeft, op "mod" (Binop Mod) AssocLeft],
             [op "+" (Binop Plus) AssocLeft, op "-" (Binop Minus) AssocLeft],
             [op "==" (Binop Equiv) AssocLeft, op "&&" (Binop And) AssocLeft, op "||" (Binop Or) AssocLeft, op "<" (Binop Less) AssocLeft, op ">" (Binop Greater) AssocLeft]
            ]
              -- and so on

-- | Parser for the 'Expr' datatype
expr :: Parser Expr
expr = buildExpressionParser operators atom

-- | Helper function for handling operators
op s f assoc = Infix (reservedOp s *> pure f) assoc

lexeme = P.lexeme lexer
integer = P.integer lexer
reserved = P.reserved lexer
parens = P.parens lexer
identifier = P.identifier lexer

-- | Ensure that the object parsed isn't already in state
new x = do
  ids' <- getids
  parsed <- x
  if parsed `elem` ids'
    then unexpected $ parsed ++ " has already been defined."
    else addid parsed >> return parsed
capIdentifier = lexeme ((:) <$> upper <*> (many alphaNum))
commaSep1 = P.commaSep1 lexer
reservedOp = P.reservedOp lexer
charLiteral = P.charLiteral lexer
comma = P.comma lexer

-- | Atomic expressions
atom :: Parser Expr
atom =
  HE <$> ((char '?') *> identifier)
  <|>
  I <$> integer
  <|>
  B <$> (reserved "True" *> pure True)
  <|>
  B <$> (reserved "False" *> pure False)
  <|>
  (try $ App <$> identifier <*> (parens (commaSep1 expr)))
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
  <?> "Parse error, expected expression"


-- | Equations
equation :: Parser Equation
equation =
  (try $ (Veq <$> identifier <*> (reservedOp "=" *> expr)))
  <|>
  (try $ do
    name <- identifier
    params <- parens (commaSep1 identifier)
    putWhileNames (name, params)
    reservedOp "="
    e <- expr
    return $ Feq name (Pars params) e)

position :: Parser Pos
position =
   Index <$> fromIntegral <$> integer -- TODO: better way?
   <|>
   identifier *> pure ForAll

-- | Board equations
boardeqn :: Parser BoardEq
boardeqn =
   (try $ (PosDef <$> identifier <*> (lexeme ((lexeme (char '!')) *> char '(') *> lexeme position) <*> (lexeme comma *> lexeme position <* lexeme (char ')')) <*> (reservedOp "=" *> expr)))

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
  reserved "Position" *> pure Position
  <|>
  reserved "Positions" *> pure Positions
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
-- >>> parseAll ttype "" "(Board, Position)" == Right (Tup [X Board S.empty, X Position S.empty])
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
valdef :: Parser ValDef
valdef = do
  s <- sig
  b <- getCtype
  case b of
    Just (Plain (X Board set)) | S.null set  -> (BVal s) <$> (boardeqn)
    _ -> (Val s) <$> (equation)

decl :: Parser (Maybe ValDef)
decl = (try $ (((,) <$> (reserved "type" *> new identifier) <*> (reservedOp "=" *> xtype)) >>= addSyn) >> return Nothing)
       <|> Just <$> valdef





-- | Board definition
board :: Parser BoardDef
board =
  (reserved "type" *> reserved "Board" *> reservedOp "=") *>
  (BoardDef <$>
    ((,) <$> (reserved "Grid" *> (lexeme . char) '(' *> (fromInteger <$> integer)) <*>
     ((lexeme . char) ',' *> (fromInteger <$> integer) <* (lexeme . char) ')')) <*>
  (reserved "of" *> xtype)) -- fixme

-- | Input definition
input :: Parser InputDef
input =
  reserved "type" *> reserved "Input" *> reservedOp "=" *> (InputDef <$> xtype)

-- | Game definition
game :: Parser Game
game =
  Game <$> (reserved "game" *> identifier) <*> board <*> input <*> (catMaybes <$> (many decl))

-- | Uses the parser p to parse all input, throws an error if anything is left over
parseAll :: Parser a -> String -> String -> Either ParseError a
parseAll p s = runParser (p <* eof) emptyState s

-- | Read from the file, and parse
parseFromFile p fname = do
  input <- readFile fname
  return (parseAll p fname input)
 
parseLine :: String -> Either ParseError Expr
parseLine = parseAll expr  ""

-- | Parse a game file, displaying an error if there's a problem (used in repl)
parseGameFile :: String -> IO (Maybe Game)
parseGameFile f = do
  parsed <- Parser.Parser.parseFromFile game f
  case parsed of
    Left err -> (putStrLn $ show err) >> return Nothing
    Right g -> return (Just g)
