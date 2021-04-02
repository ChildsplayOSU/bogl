{-|
Description : Utilities for interfacing with the type checker
-}

module Typechecker.Utils where

import Parser.Parser
import Language.Types
import Language.Syntax
import Typechecker.Typechecker
import Error.TypeError
import Error.Error
import Text.Parsec.Pos

-- | Type checks an unparsed expression string es in the game at path fp
--   A wrapper function for easier testing and debugging, not needed for typechecking
tcInGame :: FilePath -> ExprS -> IO (Either Error Xtype)
tcInGame fp es = do
   g  <- parseGameFile fp
   case (g, parseLine es) of
      (Right gp, Right ep) -> return $ tcInGame' gp ep
      _                    -> return $ Left (cterr (Unknown "parse error") (initialPos ""))
      -- If there is a parse error, the line above reports it as a dummy type error

-- | Type checks an expression in a given game
--   A wrapper function for easier testing and debugging, not needed for typechecking
tcInGame' :: (Game SourcePos) -> (Expr SourcePos) -> Either Error Xtype
tcInGame' g ex = case tcexpr (e (tc g)) ex of
                    (Right (x, _)) -> Right x
                    (Left terr)    -> Left terr
