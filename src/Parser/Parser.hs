-- | Parser for BOGL 

module Parser.Parser where

import Language.Syntax
import Debug.Trace(trace)

import Text.ParserCombinators.Parsec hiding (Parser)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language
import Text.Parsec.Expr
import qualified Text.Parsec as Par
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.Combinator
-- FIXME why am I using both parsec2 and parsec3?

import Data.Either 
type Parser = Par.Parsec String (Maybe Type)

types = ["Bool", "Int", "Symbol", "Input", "Board", "Player", "Position", "Positions"]
lexer = P.makeTokenParser (haskellStyle {P.reservedNames = ["if", "then", "True", "False",
                                                            "let", "in", "if", "then", "else",
                                                            "while", "do", "game", "type", "Grid", "of"
                                                            -- "A", "B", "free", "place", "next", "isFull", "inARow",
                                                            -- "countBoard", "countColumn", "countRow" ]
                                                            ] ++ types,
                                        P.reservedOpNames = ["=", "*", "==", "-", "/=", "/", "+", ":", "->"]})



operators = [[op "*" (Binop Times) AssocLeft, op "/" (Binop Div) AssocLeft, op "mod" (Binop Mod) AssocLeft],
             [op "+" (Binop Plus) AssocLeft, op "-" (Binop Minus) AssocLeft],
             [op "==" (Binop Equiv) AssocLeft]
            ]
              -- and so on

expr :: Parser Expr
expr = buildExpressionParser operators atom

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
  Tuple <$> parens (commaSep1 expr)
  <|>
  parens expr -- parenthesised expression
  <|>
  Let <$> (reserved "let" *> identifier) <*> (reservedOp "=" *> expr) <*> (reserved "in" *> expr)
  <|>
  If <$> (reserved "if" *> expr) <*> (reserved "then" *> expr) <*> (reserved "else" *> expr)
  <|>
  While <$> (reserved "while" *> expr) <*> (reserved "do" *> expr)
  <|>
  expr


equation :: Parser Equation
equation =
  (try $ (Veq <$> identifier <*> (reservedOp "=" *> expr)))
  <|>
  (try $ (Feq <$> identifier <*> (Pars <$> parens (commaSep1 (identifier))) <*> (reservedOp "=" *> expr)))

boardeqn :: Parser BoardEq
boardeqn =
  (try $ (RegDef <$> identifier <*> (parens expr) <*> (reservedOp "=" *> expr)))
  <|>
  (try $ (PosDef <$> identifier <*> (char '(' *> integer) <*> (integer <* char ')') <*> (reservedOp "=" *> expr)))

btype :: Parser Btype
btype =
  (reserved "Bool" *> pure Booltype
  <|>
  reserved "Int" *> pure Itype
  <|>
  reserved "Symbol" *> pure Symbol
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

xtype :: Parser Xtype
xtype =
  (try $ (X <$> btype <*> (many1 (reservedOp "|" *> identifier))))
  <|>
  (\x -> X x []) <$> btype

-- |
--
-- >>> runParser ttype Nothing "" "(Board, Position)"
-- Right (Board,Position) 
--
-- >>> runParser ttype Nothing "" "(Symbol,Board)"
-- Right (Symbol,Board) 
--
-- >>> isLeft $ runParser ttype Nothing "" "(Symbol)" 
-- True  
--
-- >>> isLeft $ runParser ttype Nothing "" "(3)" 
-- True 
ttype :: Parser Tuptype
--ttype = Tup <$> parens (commaSep1 xtype) -- this should only work for k>=2.
ttype = Tup <$> parens (lexeme ((:) <$> (xtype <* comma) <*> (commaSep1 xtype)))

ptype :: Parser Ptype
ptype = (Pext <$> xtype <|> Pt <$> ttype)

ftype :: Parser Ftype
ftype = Ft <$> ptype <*> (reservedOp "->" *> ptype)

typ :: Parser Type
typ = 
  (try $ (do
      f <- ftype
      Par.putState (Just $ Function f)
      return (Function f)
  ))
  <|>
  (try $ (do
            p <- ptype
            Par.putState (Just $ Plain p)
            return (Plain p)))
   

sig :: Parser Signature
sig =
  Sig <$> identifier <*> (reservedOp ":" *> typ)

valdef :: Parser ValDef
valdef = do
  s <- sig
  b <- getState
  case b of
    Just (Plain (Pext (X Board []))) -> (BVal s) <$> (boardeqn)
    _ -> (Val s) <$> (equation)

-- |
-- note: Empty is currently parsed as a string in the grammar, not as a Name. Is it an issue? 
-- >>> :{ 
--     runParser valdef Nothing "" ex1 == 
--       Right (Val (Sig "isValid" (Function (Ft (Pt (Tup [X Board [], X Position []])) 
--       (Pext (X Booltype []))))) (Feq "isValid" (Pars ["b", "p"]) 
--       (If (Binop Equiv (App "b" [Ref "p"]) (S "Empty")) (B True) (B False)))) 
-- :}
-- True 
ex1 = "isValid : (Board,Position) -> Bool\n  isValid(b,p) = if b(p) == Empty then True else False"

ex2 = "outcome : (Board,Player) -> Player|Tie \
\ outcome(b,p) = if inARow(3,A,b) then A else \
               \ if inARow(3,B,b) then B else \
               \ if isFull(b)     then Tie"
board :: Parser BoardDef
board =
  (reserved "type" *> reserved "Board" *> reservedOp "=") *>
  (BoardDef <$> (reserved "Grid" *> (lexeme . char) '(' *> integer) <*>
   ((lexeme . char) ',' *> integer <* (lexeme . char) ')') <*>
   (reserved "of" *> typ)) -- fixme

input :: Parser InputDef
input =
  reserved "type" *> reserved "Input" *> reservedOp "=" *> (InputDef <$> typ)

game :: Parser Game
game =
  Game <$> (reserved "game" *> identifier) <*> board <*> input <*> (many valdef)

parseFromFile p fname
   = do{ input <- readFile fname
       ; return (runParser p Nothing fname input)
       }
