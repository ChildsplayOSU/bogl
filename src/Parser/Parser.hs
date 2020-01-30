-- | Parser for BOGL 

module Parser.Parser (parseLine, parseGameFile, expr) where

import Language.Syntax
import Debug.Trace(trace)

import Text.ParserCombinators.Parsec hiding (Parser)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language
import Text.Parsec.Expr
import qualified Text.Parsec as Par
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.Combinator
import qualified Data.Set as S

-- FIXME why am I using both parsec2 and parsec3?

import Data.Either
type Parser = Par.Parsec String ((Maybe Type), (Name, [Name]))
-- | The 'Type' keywords
types = ["Bool", "Int", "Symbol", "Input", "Board", "Player", "Position", "Positions"]
-- | The lexer, using the reserved keywords and operation names
lexer = P.makeTokenParser (haskellStyle {P.reservedNames = ["if", "then", "True", "False",
                                                            "let", "in", "if", "then", "else",
                                                            "while", "do", "game", "type", "Grid", "of", "case"
                                                            -- "A", "B", "free", "place", "next", "isFull", "inARow",
                                                            -- "countBoard", "countColumn", "countRow" ]
                                                            ] ++ types,
                                        P.reservedOpNames = ["=", "*", "==", "-", "/=", "/", "+", ":", "->", "<"]})


-- | Operators (might want to fix the order of operations)
operators = [[op "*" (Binop Times) AssocLeft, op "/" (Binop Div) AssocLeft, op "mod" (Binop Mod) AssocLeft],
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
builtin = choice (map (lexeme) [string "or", string "inARow"])
identifier = P.identifier lexer
capIdentifier = lexeme ((:) <$> upper <*> (many alphaNum))
commaSep1 = P.commaSep1 lexer
reservedOp = P.reservedOp lexer
charLiteral = P.charLiteral lexer
comma = P.comma lexer 


-- | Atomic expressions
atom :: Parser Expr
atom =
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
  <|> -- a more robust macro system would be nice.
  (do
      reserved "while"
      c <- identifier <* parens (commaSep1 identifier) 
      reserved "do" 
      e <- identifier <* parens (commaSep1 identifier)
      recurse <- (fst . snd) <$> Par.getState
      i <- (snd . snd) <$> Par.getState
      let args = map Ref i
      let args' = case args of
            [x] -> x
            xs -> Tuple xs
      return $ If (App c args) (App recurse [(App e args)]) (args')) -- that list is going to break things.
        <|>
  Case <$> (reserved "case" *> identifier) <*> (reserved "of" *> many1 ((,) <$> capIdentifier <*> (reservedOp "->" *> expr))) <*> (reservedOp "|" *> expr)

-- | Equations
equation :: Parser Equation
equation =
  (try $ (Veq <$> identifier <*> (reservedOp "=" *> expr)))
  <|>
  (try $ do
    name <- identifier
    params <- parens (commaSep1 identifier)
    Par.modifyState (replaceSecond (name, params))
    reservedOp "="
    e <- expr
    return $ Feq name (Pars params) e)
 
-- | Board equations
boardeqn :: Parser BoardEq
boardeqn =
  (try $ (RegDef <$> identifier <*> (parens expr) <*> (reservedOp "=" *> expr)))
  <|>
  (try $ (PosDef <$> identifier <*> (char '(' *> expr) <*> (comma *> expr <* char ')') <*> (reservedOp "=" *> expr)))

-- | Atomic types
btype :: Parser Btype
btype =
  (reserved "Bool" *> pure Booltype
  <|>
  reserved "Int" *> pure Itype
  <|>
  reserved "Input" *> pure Input
  <|>
  do
    reserved "Board"
    pure Board
  <|>
  reserved "Player" *> pure Player
  <|>
  reserved "Position" *> pure Position
  <|>
  reserved "Positions" *> pure Positions)
  <|>
  Symbol <$> capIdentifier

-- | Extended types: types after the first are restricted to symbols
xtype :: Parser Xtype
xtype =
  (try $ (X <$> btype <*> (S.fromList <$> (many1 (reservedOp "|" *> capIdentifier)))))
  <|>
  (\x -> X x S.empty) <$> btype

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

-- | Tuple types
ttype :: Parser Tuptype
--ttype = Tup <$> parens (commaSep1 xtype) -- this should only work for k>=2.
ttype = Tup <$> parens (lexeme ((:) <$> (xtype <* comma) <*> (commaSep1 xtype)))

-- | Plain types
ptype :: Parser Ptype
ptype = (Pext <$> xtype <|> Pt <$> ttype)

-- | Function types
ftype :: Parser Ftype
ftype = Ft <$> ptype <*> (reservedOp "->" *> ptype)

replaceFirst x (a, b) = (x, b)
replaceSecond x (a, b) = (a, x)


-- | 'Type's
typ :: Parser Type
typ = 
  (try $ (do
      f <- ftype
      Par.modifyState (replaceFirst (Just $ Function f))
      return (Function f)
  ))
  <|>
  (try $ (do
            p <- ptype
            Par.modifyState (replaceFirst $ Just $ Plain p)
            return (Plain p)))
   
-- | Value signatures
sig :: Parser Signature
sig =
  Sig <$> identifier <*> (reservedOp ":" *> typ)

-- | Value definitions
valdef :: Parser ValDef
valdef = do
  s <- sig
  b <- fst <$> getState
  case b of
    Just (Plain (Pext (X Board set))) | S.null set  -> (BVal s) <$> (boardeqn)
    _ -> (Val s) <$> (equation)

-- |
-- note: Empty is currently parsed as a string in the grammar, not as a Name. Is it an issue? 
-- >>> :{ 
--     parseAll valdef "" ex1 == 
--       Right (Val (Sig "isValid" (Function (Ft (Pt (Tup [X Board S.empty, X Position S.empty])) 
--       (Pext (X Booltype S.empty))))) (Feq "isValid" (Pars ["b", "p"]) 
--       (If (Binop Equiv (App "b" [Ref "p"]) (S "Empty")) (B True) (B False)))) 
-- :}
-- True 
ex1 = "isValid : (Board,Position) -> Bool\n  isValid(b,p) = if b(p) == Empty then True else False"

ex2 = "outcome : (Board,Player) -> Player|Tie \
\ outcome(b,p) = if inARow(3,A,b) then A else \
               \ if inARow(3,B,b) then B else \
               \ if isFull(b)     then Tie"
-- | Board definition
board :: Parser BoardDef
board =
  (reserved "type" *> reserved "Board" *> reservedOp "=") *>
  (BoardDef <$> (reserved "Grid" *> (lexeme . char) '(' *> (fromInteger <$> integer)) <*>
   ((lexeme . char) ',' *> (fromInteger <$> integer) <* (lexeme . char) ')') <*>
   (reserved "of" *> typ)) -- fixme

-- | Input definition
input :: Parser InputDef
input =
  reserved "type" *> reserved "Input" *> reservedOp "=" *> (InputDef <$> typ)

-- | Game definition
game :: Parser Game
game =
  Game <$> (reserved "game" *> identifier) <*> board <*> input <*> (many valdef)

-- | Uses the parser p to parse all input, throws an error if anything is left over
parseAll p = runParser (p <* eof) (Nothing, ("", []))

-- | Read from the file, and parse
parseFromFile p fname
   = do{ input <- readFile fname
       ; return (parseAll p fname input)
       }
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
