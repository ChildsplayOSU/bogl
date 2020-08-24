-- | Parser for BOGL

module Parser.Parser (
   parseLine, parsePreludeFromText, parseGameFromText, parseGameFile, parsePreludeAndGameText,
   expr, isLeft, parseAll, valdef, xtype, boardeqn, equation, decl, parseGame, typesyn, Parser)
where

import Parser.Error
import Language.Syntax hiding (input, board)
import Language.Types
import Debug.Trace(trace, traceM)
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

-- | State for the parser
data ParState =
   PS {
       ctype      :: Maybe Type,       -- ^ type of current object being parsed
       whileNames :: (Name, [Name]),   -- ^ arguments to the last-parsed function, needed for While
       ids        :: [Name],           -- ^ identifiers
       syn        :: [(Name, Xtype)]   -- ^ type synonyms
      }

-- | A parse context with builtins
startState = PS Nothing ("", []) (map fst builtins ++ map fst builtinRefs) []

type Parser = Parsec String (ParState)

type ParseResult = Either ParseError (Game SourcePos)

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
lexer = P.makeTokenParser (haskellStyle
   {P.reservedNames  =
                        ["True", "False",
                         "let", "in", "if", "then", "else",
                         "while", "do", "game", "type", "Array", "of", "case", "type"
                        ] ++ types,
    P.reservedOpNames = ["=", "*", "<", "<=", "==", "/=", ">", ">=", "-", "/=",
                         "/", "+", ":", "->", "{", "}"]
    })

-- | Operators (might want to fix the order of operations)
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

-- | Ensure the object doesn't exist, but don't add it in the process
notAlreadyInUse x = do
  ids' <- getids
  parsed <- x
  if parsed `elem` ids'
    then unexpected $ "redefinition of " ++ parsed
    else return parsed

-- | Identifies a valid game name, also used for types
-- nearly identical to 'capIdentifier', just includes underscores
gameIdentifierChars = ['a'..'z']++['A'..'Z']++['0'..'9']++"_"
gameIdentifier = lexeme ((:) <$> upper <*> (many (oneOf gameIdentifierChars)))

capIdentifier = gameIdentifier--lexeme ((:) <$> upper <*> (many alphaNum))
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
      c <- expr
      reserved "do"
      e <- expr
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
   name <- try(string n <* (lexeme ((lexeme (char '!')) *> char '(')))
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

enum :: Parser (S.Set Name)
enum = reservedOp "{" *>
         (S.fromList <$> (commaSep1 (notAlreadyInUse capIdentifier))) <* reservedOp "}"

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

-- | Function types
ftype :: Parser Ftype
ftype = do
  x <- xtype
  reservedOp "->"
  r <- xtype
  case x of
    Tup xs -> return $ Ft (Tup xs) r
    y -> return $ Ft (Tup [y]) r

-- | Types
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
  lookAhead lower *> (Sig <$> new identifier <*> (reservedOp ":" *> typ))

-- | Value definitions
valdef :: Parser (ValDef SourcePos)
valdef = do
  (Sig n t) <- sig
  b <- getCtype
  case b of
    Just (Plain (X Board set))
      | S.null set -> (BVal (Sig n t)) <$> many1 (boardeqn n) <*> getPosition
    _ -> (Val (Sig n t)) <$> (equation) <*> getPosition

decl :: Parser (Maybe (ValDef SourcePos))
decl = typesyn *> return Nothing
       <|> Just <$> valdef

-- | Board definition
board :: Parser BoardDef
board = do
  -- attempt to parse a regular board def
  -- if the words 'type Board' are seen, assume we are parsing a board
  -- otherwise fall to the alternative below with a default board instead
  (try (reserved "type" *> reserved "Board") *> reservedOp "=")
     *> (reserved "Array" *> (lexeme . char) '(')
  x <- int
  (lexeme . char) ','
  y <- int
  (lexeme . char) ')'
  boardType <- reserved "of" *> xtype
  guard (x > 0 && y > 0) <?> "board dimensions to be >= 1"
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
  (reserved "type" *> lookAhead upper *> new identifier) <*>
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
  ((\p -> vs ++ catMaybes p) <$> (many decl))

-- | Uses the parser p to parse all input with state ps, throws an error if anything is left over
parseWithState :: ParState -> Parser a -> String -> String -> Either ParseError a
parseWithState ps p s = runParser (p <* eof) ps s

-- | Uses the parser p to parse all input, throws an error if anything is left over
parseAll :: Parser a -> String -> String -> Either ParseError a
parseAll = parseWithState startState

-- | Read from the file, and parse
parseFromFile :: Parser a -> FilePath -> IO (Either ParseError a)
parseFromFile p fname = do
  input <- readFile fname
  return (parseAll p fname input)

-- | Parse from text directly w/out a file
-- Still takes a file name so as to provide a reasonable debug message if parsing fails
-- This will likely be something general, such as 'Prelude' or 'Gamefile'
parseFromText :: Parser a -> String -> String -> Either ParseError a
parseFromText p fn content = parseAll p fn content

parseLine :: String -> Either ParseError (Expr SourcePos)
parseLine = parseAll expr ""

parseGameFile :: String -> IO (ParseResult)
parseGameFile = parseFromFile (parseGame [])

-- | Parse the prelude from text
parsePreludeFromText :: String -> Either ParseError ([Maybe (ValDef SourcePos)], ParState)
parsePreludeFromText content = parseFromText prelude "Prelude" content

-- | Parse a game from text and the result of a previous parse (e.g. the prelude)
-- Such as in the case of the function above 'parsePreludeFromtext'
parseGameFromText :: String -> ([Maybe (ValDef SourcePos)], ParState) -> ParseResult
parseGameFromText prog pr = parseWithState (snd pr) (parseGame (catMaybes (fst pr))) "Code" prog

-- | Parse a prelude and game from text directly, without a file
parsePreludeAndGameText :: String -> String -> IO ParseResult
parsePreludeAndGameText preludeContent gameFileContent = do
  prel <- return (parsePreludeFromText preludeContent)
  case prel of
    Right r -> return (parseGameFromText gameFileContent r)
    Left err              -> return $ Left err
