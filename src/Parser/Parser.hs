-- | Parser for BOGL 

module Parser.Parser (parseLine, parseGameFile, expr) where

import Language.Syntax hiding (input, board)
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
builtin = choice (map (lexeme) [string "or", string "inARow"])
identifier = choice ((map (try . lexeme) [string "or", string "inARow"]) ++ [P.identifier lexer])
--identifier = (builtin <|> P.identifier lexer)
newIdentifier = P.identifier lexer                             -- must not be a built in identifier (e.g. in a let expr) 
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
  Let <$> (reserved "let" *> newIdentifier) <*> (reservedOp "=" *> expr) <*> (reserved "in" *> expr)
  <|>
  If <$> (reserved "if" *> expr) <*> (reserved "then" *> expr) <*> (reserved "else" *> expr)
  <|>
  (do
      reserved "while"
      c <- atom
      reserved "do" 
      e <- atom 
      recurse <- (fst . snd) <$> Par.getState
      names <- (snd . snd) <$> Par.getState -- get the names of the parameters to the function which wraps this while 
      let exprs = map Ref names
      let exprs' = case exprs of
            [x] -> x
            xs -> Tuple xs
      return $ While c e names exprs') 
   <|>
  Case <$> (reserved "case" *> newIdentifier) <*> (reserved "of" *> many1 ((,) <$> capIdentifier <*> (reservedOp "->" *> expr))) <*> (reservedOp "|" *> expr)

-- | Equations
equation :: Parser Equation
equation =
  (try $ (Veq <$> newIdentifier <*> (reservedOp "=" *> expr)))
  <|>
  (try $ do
    name <- newIdentifier
    params <- parens (commaSep1 newIdentifier)
    Par.modifyState (replaceSecond (name, params))
    reservedOp "="
    e <- expr
    return $ Feq name (Pars params) e)

position :: Parser Pos 
position = 
   Index <$> fromIntegral <$> integer -- TODO: better way?  
   <|>
   newIdentifier *> pure ForAll

-- | Board equations
boardeqn :: Parser BoardEq
boardeqn =
   (try $ (PosDef <$> newIdentifier <*> (lexeme ((lexeme (char '!')) *> char '(') *> lexeme position) <*> (lexeme comma *> lexeme position <* lexeme (char ')')) <*> (reservedOp "=" *> expr)))

-- | Atomic types
btype :: Parser Btype
btype =
  reserved "Bool" *> pure Booltype
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
            p <- xtype
            Par.modifyState (replaceFirst $ Just $ Plain p)
            return (Plain p)))
   
-- | Value signatures
sig :: Parser Signature
sig =
  Sig <$> newIdentifier <*> (reservedOp ":" *> typ)

-- | Value definitions
valdef :: Parser ValDef
valdef = do
  s <- sig
  b <- fst <$> getState
  case b of
    Just (Plain (X Board set)) | S.null set  -> (BVal s) <$> (boardeqn)
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
  Game <$> (reserved "game" *> newIdentifier) <*> board <*> input <*> (many valdef)

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
