-- | Parser for BOGL 

module Parser.Parser (parseLine, parseGameFile, expr) where

import Language.Syntax hiding (input, board)
import Language.Types
import Debug.Trace(trace)

import qualified Text.Parsec.Token as P

import Text.Parsec.Language
import Text.Parsec.Expr

import Text.Parsec
import qualified Data.Set as S

import Data.Either

data ParState = PS {
  ctype :: Maybe Type,
  whileNames :: (Name, [Name]),
  ids :: [Name]
                   }

emptyState = PS Nothing ("", []) []

type Parser = Parsec String (ParState)

getids :: Parser [Name]
getids = ids <$> getState

addid :: Name -> Parser ()
addid n = do
  PS c w ids <- getState
  putState (PS c w (n:ids))

getWhileNames :: Parser (Name, [Name])
getWhileNames = whileNames <$> getState

putWhileNames :: (Name, [Name]) -> Parser ()
putWhileNames n = do
  PS c w ids <- getState
  putState (PS c n ids)

getCtype :: Parser (Maybe Type)
getCtype = ctype <$> getState

putType :: Type -> Parser ()
putType t = do
  PS c w ids <- getState
  putState (PS (Just t) w ids)
-- | The 'Type' keywords
types = ["Bool", "Int", "AnySymbol", "Input", "Board", "Player", "Position", "Positions"]
-- | The lexer, using the reserved keywords and operation names
lexer = P.makeTokenParser (haskellStyle {P.reservedNames = ["if", "then", "True", "False",
                                                            "let", "in", "if", "then", "else",
                                                            "while", "do", "game", "type", "Grid", "of", "case"
                                                            --"place", "remove", "isFull", "inARow", "at"
                                                            -- "A", "B", "free", "place", "next", "isFull", "inARow",
                                                            -- "countBoard", "countColumn", "countRow" ]
                                                            ] ++ types,
                                        P.reservedOpNames = ["=", "*", "==", "-", "/=", "/", "+", ":", "->", "<"]})



-- | Operators (might want to fix the order of operations)
operators = [
             [op "!" (Binop Get) AssocLeft],
             [op "*" (Binop Times) AssocLeft, op "/" (Binop Div) AssocLeft, op "mod" (Binop Mod) AssocLeft],
             [op "+" (Binop Plus) AssocLeft, op "-" (Binop Minus) AssocLeft],
             [op "==" (Binop Equiv) AssocLeft, op "&&" (Binop And) AssocLeft, op "||" (Binop Or) AssocLeft, op "<" (Binop Less) AssocLeft, op ">" (Binop Greater) AssocLeft]
            ]
              -- and so on

-- | Parser for the 'Expr' datatype
--
-- >>> parseLine' expr "40 + 2" == Right (Binop Plus (I 40) (I 2))  
-- True 
--
-- >>> isLeft $ parseLine' expr "40 + 2life,the universe, and everything" 
-- True 
expr :: Parser Expr
expr = buildExpressionParser operators atom

-- | Helper function for handling operators
op s f assoc = Infix (reservedOp s *> pure f) assoc

lexeme = P.lexeme lexer
integer = P.integer lexer
reserved = P.reserved lexer
parens = P.parens lexer
identifier = P.identifier lexer

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
   (try $ (PosDef <$> identifier <*> (lexeme (char '(') *> lexeme position) <*> (lexeme comma *> lexeme position <* lexeme (char ')')) <*> (reservedOp "=" *> expr)))

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


-- | Extended types: types after the first are restricted to symbols
xtype :: Parser Xtype
xtype =
  (try $ (X <$> btype <*> (S.fromList <$> many1 (reservedOp "|" *> capIdentifier))))
  <|>
  (try $ (\x -> X x S.empty) <$> btype)
  <|>
  (try $ (pure $ X Top) <*> (S.union <$> (S.singleton <$> capIdentifier) <*> (S.fromList <$> many (reservedOp "|" *> capIdentifier))))
  <|>
  Tup <$> parens (lexeme ((:) <$> (xtype <* comma) <*> (commaSep1 xtype)))

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
  Game <$> (reserved "game" *> identifier) <*> board <*> input <*> (many valdef)

-- | Uses the parser p to parse all input, throws an error if anything is left over
parseAll :: Parser a -> String -> String -> Either ParseError a
parseAll p s = runParser (p <* eof) emptyState s

-- | Read from the file, and parse
parseFromFile p fname = do
  input <- readFile fname
  return (parseAll p fname input)
 
parseLine :: String -> Either ParseError Expr
parseLine = parseAll expr  ""

-- | Read a single line and return the result (intended for brevity in test cases)
parseLine' :: Parser a -> String -> Either ParseError a
parseLine' p = parseAll p ""

-- | Parse a game file, displaying an error if there's a problem (used in repl)
parseGameFile :: String -> IO (Maybe Game)
parseGameFile f = do
  parsed <- Parser.Parser.parseFromFile game f
  case parsed of
    Left err -> (putStrLn $ show err) >> return Nothing
    Right g -> return (Just g)
