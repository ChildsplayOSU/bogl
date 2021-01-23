{-|
Module      : Parser.Parser
Description : Parser for BoGL
Copyright   : (c)
License     : BSD-3
-}

module Parser.Parser (
   parseLine, parseFromText, parsePreludeFromText, parseGameFromText, parseGameFile, parsePreludeAndGameText,
   expr, isLeft, parseAll, valdef, ftype, xtype, boardeqn, equation, decl, parseGame, typesyn, Parser, lexer, reservedNames, enum, literal)
where

import Parser.Error
import Language.Syntax hiding (input, board)
import Language.Types

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

import Data.Functor.Identity(Identity)

-- | State for the parser
data ParState =
   PS {
       ctype      :: Maybe Type,       -- ^ type of current object being parsed
       whileNames :: [Name],           -- ^ context from last-parsed function or let expression
       ids        :: [Name],           -- ^ identifiers
       tdef      :: [TypeDef]         -- ^ type definitions
      }

-- | A parse context with builtins
startState :: ParState
startState = PS Nothing [] (map fst builtins ++ map fst builtinRefs ++ reservedTypes) []

-- | Parser type
type Parser = Parsec String (ParState)

-- | Result of parsing
type ParseResult = Either ParseError (Game SourcePos)

-- | Get all used ids
getids :: Parser [Name]
getids = ids <$> getState

-- | Get all type definitions
getDefs :: Parser [TypeDef]
getDefs = tdef <$> getState

-- | Add a type synonym
addSyn :: (Name, Xtype) -> Parser ()
addSyn x = modifyState (\env -> env{tdef = x:tdef env})

-- | Lookup a type synonym
lookupSyn :: Name -> Parser Xtype
lookupSyn n = do
  t <- (lookup <$> (pure n) <*> (tdef <$> getState))
  case t of
    Nothing -> fail $ "Type " ++ n ++ " not declared!"
    Just t' -> return t'

-- | Add an id to list of used ids
addid :: Name -> Parser ()
addid n = do
  PS c w ids' x <- getState
  putState (PS c w (n:ids') x)

-- | Get the names necessary to build 'While'
getWhileNames :: Parser [Name]
getWhileNames = whileNames <$> getState

-- | Put the names
putWhileNames :: [Name] -> Parser ()
putWhileNames n = do
  PS c _ ids' x <- getState
  putState (PS c n ids' x)

-- | Get the current type of the object being parsed (DEPRECATED due to new board syntax)
getCtype :: Parser (Maybe Type)
getCtype = ctype <$> getState

-- | Put the type of the object being parsed
putType :: Type -> Parser ()
putType t = do
  PS _ w ids' x <- getState
  putState (PS (Just t) w ids' x)

-- | The 'Type' keywords
types :: [String]
types = ["Bool", "Int", "AnySymbol", "Input", "Board"]

-- | The lexer, using the reserved keywords and operation names
lexer :: P.GenTokenParser String u Data.Functor.Identity.Identity
lexer = P.makeTokenParser boglDef

-- | The lexical definitions of bogl
boglDef :: GenLanguageDef String u Identity
boglDef = (haskellStyle
   {P.reservedNames  =
                        ["True", "False",
                         "let", "in", "if", "then", "else",
                         "while", "do", "game", "type", "Array", "of", "case", "type"
                        ] ++ types,
    P.reservedOpNames = ["=", "*", "<", "<=", "==", "/=", ">", ">=", "-", "/=",
                         "/", "+", ":", "->", "{", "}"]
    })

-- | Operators (might want to fix the order of operations)
operators :: [[Operator String u Identity (Expr a)]]
operators = [
             [op "!" (Binop Get) AssocLeft],
             [op "*" (Binop Times) AssocLeft, op "/" (Binop Div) AssocLeft,
              op "%" (Binop Mod) AssocLeft],
             [op "+" (Binop Plus) AssocLeft, op "-" (Binop Minus) AssocLeft],
             [op "<" (Binop Less) AssocLeft],
             [op "<=" (Binop Leq) AssocLeft],
             [op "==" (Binop Equiv) AssocLeft],
             [op "/=" (Binop NotEquiv) AssocLeft],
             [op ">=" (Binop Geq) AssocLeft],
             [op ">" (Binop Greater) AssocLeft]
            ]

-- | The list of reserved names
reservedNames :: [String]
reservedNames = P.reservedNames boglDef

-- | Parser for the 'Expr' datatype
expr :: Parser (Expr SourcePos)
expr = buildExpressionParser operators atom

-- | Helper function for handling operators
op :: String -> (a -> a -> a) -> Assoc -> Operator String u Identity a
op s f assoc = Infix (reservedOp s *> pure f) assoc

lexeme :: ParsecT String u Identity a -> ParsecT String u Identity a
lexeme = P.lexeme lexer

-- | Integer token recognizer
int :: ParsecT String u Identity Int
int = fromInteger <$> P.integer lexer

-- | Reserved token recognizer
reserved :: String -> ParsecT String u Identity ()
reserved = P.reserved lexer

-- | Parentheses recognizer
parens :: ParsecT String u Identity a -> ParsecT String u Identity a
parens = P.parens lexer

-- | Identifier recognizer (lower or upper case a-z start)
identifier :: ParsecT String u Identity String
identifier = P.identifier lexer

-- | Lowercase identifier recognizer
lowerIdentifier :: ParsecT String u Identity String
lowerIdentifier = lookAhead lower *> identifier

-- | Whitespace recognizer
whiteSpace :: ParsecT String u Identity ()
whiteSpace = P.whiteSpace lexer

-- | Ensure that the object parsed isn't already in state
new :: ParsecT String ParState Identity Name -> ParsecT String ParState Identity Name
new x = do
  ids' <- getids
  parsed <- x
  if parsed `elem` ids'
    then unexpected $ "redefinition of " ++ parsed
    else addid parsed >> return parsed

-- | Ensure the object doesn't exist, but don't add it in the process
notAlreadyInUse :: ParsecT String ParState Identity Name -> ParsecT String ParState Identity Name
notAlreadyInUse x = do
  ids' <- getids
  parsed <- x
  if parsed `elem` ids'
    then unexpected $ "redefinition of " ++ parsed
    else return parsed

-- | Identifies a valid game name
gameIdentifier :: ParsecT String u Identity [Char]
gameIdentifier = capIdentifier

-- | Starting uppercase letter identifier
capIdentifier :: ParsecT String u Identity [Char]
capIdentifier = lookAhead upper *> identifier

-- | Comma separated values, 2 or more
commaSep2 :: ParsecT String u Identity a -> ParsecT String u Identity [a]
commaSep2 p = (:) <$> (lexeme p <* lexeme comma) <*> commaSep1 p

-- | Comma separated values, 1 or more
commaSep1 :: ParsecT String u Identity a -> ParsecT String u Identity [a]
commaSep1 = P.commaSep1 lexer

-- | 0 or more comma separated values
-- unused, but possibly useful
--commaSep :: ParsecT String u Identity a -> ParsecT String u Identity [a]
--commaSep = P.commaSep lexer

-- | Reserved ops
reservedOp :: String -> ParsecT String u Identity ()
reservedOp = P.reservedOp lexer

-- | Literal comopsed of characters
-- TODO REMOVED UNUSED
--charLiteral = P.charLiteral lexer

-- | Comma separator
comma :: ParsecT String u Identity String
comma = P.comma lexer

-- | A parser for a literal expression (to be used for input).
--   Consumes all preceding whitespace since it is a top-level parser.
literal :: Parser (Expr SourcePos)
literal = spaces *> -- note: intentionally does not use whiteSpace, which allows comments
  (I <$> int
  <|>
  B <$> (reserved "True" *> pure True)
  <|>
  B <$> (reserved "False" *> pure False)
  <|>
  S <$> capIdentifier
  <|>
  (try $ parens (literal <* notFollowedBy comma)) -- parenthesized literal
  <|>
  Tuple <$> parens (commaSep2 literal))

-- | Atomic expression, under an annotation
atom :: Parser (Expr SourcePos)
atom =
  Annotation <$> getPosition <*> atom'

-- | Atomic expressions
atom' :: Parser (Expr SourcePos)
atom' =
  HE <$> ((char '?') *> lowerIdentifier)
  <|>
  I <$> int
  <|>
  B <$> (reserved "True" *> pure True)
  <|>
  B <$> (reserved "False" *> pure False)
  <|>
  (try $ App <$> lowerIdentifier <*> (parens (formArgs <$> (commaSep1 expr))))
  <|>
  S <$> capIdentifier
  <|>
  Ref <$> lowerIdentifier
  <|>
  (try $ parens (expr <* notFollowedBy comma))
  <|>
  Tuple <$> parens (commaSep1 expr)
  <|>
  (do
      reserved "let"
      var <- lowerIdentifier
      reservedOp "="
      outer <- expr
      reserved "in"
      putWhileNames [var]
      inner <- expr
      return $ Let var outer inner
  )
  <|>
  If <$> (reserved "if" *> expr) <*> (reserved "then" *> expr) <*> (reserved "else" *> expr)
  <|>
  (do
      reserved "while"
      names <- getWhileNames
      c <- expr
      reserved "do"
      e <- expr
      let exprs = map Ref names
      let exprs' = case exprs of
            [x] -> x
            xs -> Tuple xs
      return $ While c e names exprs')
  <?> "expression"
      where
         formArgs :: [Expr a] -> Expr a
         formArgs [x] = x
         formArgs x   = Tuple x

-- | Parse parameters
params :: Name -> Parser [Name]
params n = do
   parameters <- parens $ commaSep1 $ lowerIdentifier
   let paramSet = nub parameters
   if paramSet == parameters
      then return parameters
      else
         let repeats = parameters \\ paramSet in unexpected $ errRepeatParam repeats n

-- | Equations,
-- reads the line's identifier, and verifies it matches the prior declaration name
-- before proceeding
equation :: String -> Parser (Equation SourcePos)
equation eqname = do
  lexeme $ string eqname
  (varEquation eqname) <|> (funcEquation eqname)

-- | Variable Equations
varEquation :: String -> Parser (Equation SourcePos)
varEquation eqname = (try $ do
  reservedOp "="
  e <- expr
  return $ Veq eqname e)

-- | Function Equations
funcEquation :: String -> Parser (Equation SourcePos)
funcEquation eqname = (try $ do
  _params <- params eqname
  putWhileNames _params
  reservedOp "="
  e <- expr
  return $ Feq eqname (Pars _params) e)

-- | Parse a position
position :: Parser Pos
position =
   Index <$> int
   <|>
   ForAll <$> lowerIdentifier

-- | Board equations
boardeqn :: String -> Parser (BoardEq SourcePos)
boardeqn n = do
   _name <- try(string n <* (lexeme ((lexeme (char '!')) *> char '(')))
   _xpos <- lexeme position <* lexeme comma
   _ypos <- lexeme position <* (lexeme (char ')') <* reservedOp "=")
   case (_xpos, _ypos) of
      (ForAll xn, ForAll yn) -> if xn == yn then unexpected (errRepeatParam [xn] _name)
         else do
            _exp <- expr
            return $ PosDef _name _xpos _ypos _exp
      _ -> do
            _exp <- expr
            return $ PosDef _name _xpos _ypos _exp

-- | Atomic types
btype :: Parser Btype
btype =
  reserved "Bool" *> pure Booltype
  <|>
  reserved "Int" *> pure Itype
  -- removes 'Input' as a directly usable type
  -- But can still be properly set via 'type Input = ...'
  -- <|>
  --reserved "Input" *> pure Input
  <|>
  reserved "Board" *> pure Board
  -- removes 'AnySymbol' as a directly usable type
  -- serves as a parent of all types internally, but this prevents
  -- it from being referenced externally (as requested by the CSforAll group) (@montymxb)
  -- <|>
  -- reserved "AnySymbol" *> pure AnySymbol

-- | Parse an enum of a set of names
enum :: Parser (S.Set Name)
enum = reservedOp "{" *>
         (S.fromList <$> (commaSep1 (notAlreadyInUse capIdentifier))) <* reservedOp "}"

-- | An enum or the type name of an enum
--   in the future, this will be an enum or any type name
--   the type checker should catch the error if it is not an enum
--   that currently would require a substantial change that will be better to implemenent when the
--   new type system is implemented
enumName :: Parser (S.Set Name)
enumName =
   enum
   <|>
      (do
         maybeE <- typeName
         case maybeE of          -- todo: move this to the type checker
            X Top e -> return e
            _ -> fail "the right side of & must either be an {Enumeration} or the name of one"
      )

-- | Parse a type name if it has been defined
typeName :: Parser Xtype
typeName = capIdentifier >>= lookupSyn

-- | Extended types: types after the first are restricted to symbols
xtype :: Parser Xtype
xtype = (try $ do
  x1 <- xtype'
  reservedOp "&"
  enum' <- enumName
  case x1 of
    (X b xs) -> return (X b (S.union xs enum'))
    a -> return a)
  <|>
      try (xtype' <* (notFollowedBy (string "&")))

-- | Parse just a type, before it is possibly extended
xtype' :: Parser Xtype
xtype' =
  (try typeName)
  <|>
  (try $ X <$> (pure Top) <*> enum) -- Plain enum
  <|>
  (try $ X <$> btype <*> (pure S.empty))
  <|>
  (try $ Tup <$> parens (lexeme ((:) <$> (xtype <* comma) <*> (commaSep1 xtype))))

-- | Parse an xtype, but only if it is one of the base types or it has been declared
--   and named (with the type keyword)
namedType :: Parser Xtype
namedType =
   (try $ notExtended typeName)
   <|>
   (try $ Tup <$> parens (lexeme ((:) <$> (namedType <* comma) <*> (commaSep1 namedType))))
   <|>
   (try $ notExtended $ X <$> btype <*> pure S.empty)
   <?>
   "a base type or previously-declared type"
      where
         notExtended :: Parser a -> Parser a
         notExtended n = n <* (notFollowedBy (string "&"))

-- | Function types
ftype :: Parser Ftype
ftype = do
  x <- namedType
  reservedOp "->"
  r <- namedType
  return $ Ft x r

-- | Types that appear in signatures
typ :: Parser Type
typ =
  (try $ (do
            _p <- namedType <* notFollowedBy (reservedOp "->")
            putType (Plain _p)
            return (Plain _p)))
  <|>
  (do
      f <- ftype
      putType (Function f)
      return (Function f)
  )

-- | Value signatures
sig :: Parser Signature
sig = Sig <$> new lowerIdentifier <*> (reservedOp ":" *> typ)

-- | Value definitions
valdef :: Parser (ValDef SourcePos)
valdef = do
  (Sig n t) <- sig
  b <- getCtype
  let val = (Val (Sig n t)) <$> (equation n) <*> getPosition in
     case b of
        Just (Plain (X Board set))
           | S.null set -> ((BVal (Sig n t)) <$> many1 (boardeqn n) <*> getPosition) <|> val
        _ -> val

-- | Parse a typesyn or a valdef
decl :: Parser (Maybe (ValDef SourcePos))
decl = typesyn *> return Nothing
       <|> Just <$> valdef

-- | Board definition
board :: Parser BoardDef
board = do
  -- attempt to parse a regular board def
  -- if the words 'type Board' are seen, assume we are parsing a board
  -- otherwise fall to the alternative below with a default board instead
  _ <- (try (reserved "type" *> reserved "Board") *> reservedOp "=")
     *> (reserved "Array" *> (lexeme . char) '(')
  x <- int
  _ <- (lexeme . char) ','
  y <- int
  _ <- (lexeme . char) ')'
  boardType <- reserved "of" *> xtype
  guard (x > 0 && y > 0) <?> "board dimensions to be >= 1"
  addSyn ("Content", boardType)
  return $ BoardDef (x,y) boardType
  -- fallback to a default 1,1 board of Int
  -- only comes up when 'type Board' is not seen
 <|> return (BoardDef (1,1) (X Itype (S.fromList [])))

-- | Input definition
input :: Parser InputDef
input =
  -- attempt to parse 'type Input', to verify we are looking for input
  try(reserved "type" *> reserved "Input") *> reservedOp "=" *> (InputDef <$> xtype)
  -- fall back to a default Input type of Int
  <|> return (InputDef (X Itype (S.fromList [])))

-- | Type Synonym Definition, but returns nothing
typesyn :: Parser ()
typesyn = (try $ (((,) <$>
  (reserved "type" *> new capIdentifier) <*>
  (reservedOp "=" *> xtype)) >>= addSyn))

-- | Prelude definition
prelude :: Parser([(Maybe (ValDef SourcePos))], ParState)
prelude = do
             defs <- whiteSpace *> many decl
             s    <- getState
             return (defs, s)

-- | Game definition
parseGame :: [ValDef SourcePos] -> Parser (Game SourcePos)
parseGame vs =
  -- game name first
  Game <$> ((whiteSpace *> reserved "game") *> gameIdentifier) <*>
  -- followed by type synonyms for board and input
  (many typesyn *> board) <*> (many typesyn *> input) <*>
  -- followed by the prelude contents, and any other declarations
  ((\_p -> vs ++ catMaybes _p) <$> (many decl)) <*> getDefs

-- | Uses the parser p to parse all input with state ps, throws an error if anything is left over
parseWithState :: ParState -> Parser a -> String -> String -> Either ParseError a
parseWithState ps _p s = runParser (_p <* eof) ps s

-- | Uses the parser p to parse all input, throws an error if anything is left over
parseAll :: Parser a -> String -> String -> Either ParseError a
parseAll = parseWithState startState

-- | Read from the file, and parse
parseFromFile :: Parser a -> FilePath -> IO (Either ParseError a)
parseFromFile _p fname = do
  _input <- readFile fname
  return (parseAll _p fname _input)

-- | Parse from text directly w/out a file
-- Still takes a file name so as to provide a reasonable debug message if parsing fails
-- This will likely be something general, such as Prelude or Gamefile
parseFromText :: Parser a -> String -> String -> Either ParseError a
parseFromText _p fn contents = parseAll _p fn contents

-- | Parse a single line as an expression
parseLine :: String -> Either ParseError (Expr SourcePos)
parseLine = parseAll expr ""

-- | Parse a game from a string
parseGameFile :: String -> IO (ParseResult)
parseGameFile = parseFromFile (parseGame [])

-- | Parse the prelude from text
parsePreludeFromText :: String -> Either ParseError ([Maybe (ValDef SourcePos)], ParState)
parsePreludeFromText contents = parseFromText prelude "Prelude" contents

-- | Parse a game from text and the result of a previous parse (e.g. the prelude)
-- Such as in the case of the function above 'Parser.Parser.parsePreludeFromtext'
parseGameFromText :: String -> String -> ([Maybe (ValDef SourcePos)], ParState) -> ParseResult
parseGameFromText prog fileName pr = parseWithState (snd pr) (parseGame (catMaybes (fst pr))) fileName prog

-- | Parse a prelude and game from text directly, without a file
parsePreludeAndGameText :: String -> String -> String -> ParseResult
parsePreludeAndGameText preludeContent gameFileContent fileName =
  case parsePreludeFromText preludeContent of
    Right r  -> (parseGameFromText gameFileContent fileName r)
    Left err -> Left err
