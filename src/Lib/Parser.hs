module Lib.Parser (

) where

import Control.Monad (guard)
import Data.Data (Data (toConstr))
import Lib.Token (
  Constant (..),
  Token (..),
  TokenKind (..),
 )
import Text.Parsec (ParsecT)
import Text.Parsec.Prim (tokenPrim)

type Parser u m = ParsecT [Token] u m Token

satisfy :: (Monad m) => (Token -> Bool) -> Parser u m
satisfy f = tokenPrim show nextpos test
 where
  test x = guard (f x) >> return x
  nextpos pos _ _ = pos

parseIdentifier :: (Monad m) => Parser u m
parseIdentifier =
  satisfy
    ((== toConstr IDENT) . toConstr . tokKind)

parseConstant :: (Monad m) => Parser u m
parseConstant =
  satisfy
    ( (`elem` map toConstr [DECIMAL, OCTAL, HEX, DECIMAL_FLOAT, HEX_FLOAT])
        . toConstr
        . tokKind
    )

parseStringLiteral :: (Monad m) => Parser u m
parseStringLiteral =
  satisfy
    ((== toConstr STRING_LIT) . toConstr . tokKind)

parseParenthesized :: (Monad m) => Parser u m
parseParenthesized = undefined
