-- |

module Parser.Parser where

import Language.Syntax

import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language
import Text.Parsec.Expr


lexer = P.makeTokenParser (haskellStyle {P.reservedNames = ["if", "then", "True", "False", "let", "in", "if", "then", "else", "while", "do"],
                                        P.reservedOpNames = ["=", "*", "==", "-", "/=", "/", "+"]})



operators = [[op "*" (Binop Times) AssocLeft, op "/" (Binop Div) AssocLeft, op "mod" (Binop Mod) AssocLeft]] -- and so on

expr = buildExpressionParser operators atom

op s f assoc = Infix (reservedOp s *> pure f) assoc

lexeme = P.lexeme lexer
integer = P.integer lexer
reserved = P.reserved lexer
parens = P.parens lexer
identifier = P.identifier lexer
commaSep1 = P.commaSep1 lexer
reservedOp = P.reservedOp lexer

atom :: Parser Expr
atom =
  I <$> integer
  <|>
  B <$> (reserved "True" *> pure True)
  <|>
  B <$> (reserved "False" *> pure False)
  <|>
  N <$> identifier
  <|>
  Tuple <$> parens (commaSep1 parser)

parser :: Parser Expr
parser =
  parens parser -- parenthesised expression
  <|>
  Let <$> (reserved "let" *> identifier) <*> (reservedOp "=" *> parser) <*> (reserved "in" *> parser)
  <|>
  If <$> (reserved "if" *> parser) <*> (reserved "then" *> parser) <*> (reserved "else" *> parser)
  <|>
  While <$> (reserved "while" *> parser) <*> (reserved "do" *> parser)
  <|>
  expr
