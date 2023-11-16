{-# LANGUAGE DeriveDataTypeable #-}

module Lib.Token (
    identKind,
    isConstant,
    isKw,
    isPunctuation,
    punctuators,
    isPunctuatorStart,
    punctuatorsNext,
    Token (..),
    TokenKind (..),
    Keyword (..),
    KeywordUnderscore (..),
    KeywordExtended (..),
    Constant (..),
    Punctuation (..),
    resolveDigraph,
) where

import Data.Char (toLower, toUpper)
import Data.Data (Data, Typeable, toConstr)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

data TokenKind
    = ERROR
    | EOF
    | Keyword Keyword
    | KeywordUnderscore KeywordUnderscore
    | KeywordExtended KeywordExtended
    | Constant Constant
    | Punctuation Punctuation
    | IDENT
    deriving (Eq)

instance Show TokenKind where
    show ERROR = "ERROR"
    show EOF = "EOF"
    show (Keyword k) = "KW_" ++ show k
    show (KeywordUnderscore k) = "KWU_" ++ show k
    show (KeywordExtended k) = "KWX_" ++ show k
    show (Constant c) = "C_" ++ show c
    show (Punctuation p) = "P_" ++ show p
    show IDENT = "IDENT"

data Keyword
    = AUTO
    | BREAK
    | CASE
    | CHAR
    | CONST
    | CONTINUE
    | DEFAULT
    | DO
    | DOUBLE
    | ELSE
    | ENUM
    | EXTERN
    | FLOAT
    | FOR
    | GOTO
    | IF
    | INLINE
    | INT
    | LONG
    | REGISTER
    | RESTRICT
    | RETURN
    | SHORT
    | SIGNED
    | SIZEOF
    | STATIC
    | STRUCT
    | SWITCH
    | TYPEDEF
    | UNION
    | UNSIGNED
    | VOID
    | VOLATILE
    | WHILE
    deriving
        ( Eq
        , Show
        , Enum
        , Bounded
        , Data
        , Typeable
        )

data KeywordUnderscore
    = ALIGNAS
    | ALIGNOF
    | ATOMIC
    | BOOL
    | COMPLEX
    | GENERIC
    | IMAGINARY
    | NORETURN
    | STATIC_ASSERT
    | THREAD_LOCAL
    deriving
        ( Eq
        , Show
        , Enum
        , Bounded
        , Data
        , Typeable
        )

data KeywordExtended
    = MACRO
    | IMPORT
    | AS
    deriving
        ( Eq
        , Show
        , Enum
        , Bounded
        , Data
        , Typeable
        )

capitalizeFirst :: String -> String
capitalizeFirst [] = []
capitalizeFirst (x : xs) = toUpper x : map toLower xs

tokenKindMap :: Map String TokenKind
tokenKindMap = Map.fromList $ keywordList ++ keywordUnderscoreList ++ keywordExtendedList
  where
    keywordList =
        [ (map toLower (show k), Keyword k) | k <- [minBound .. maxBound]
        ]
    keywordUnderscoreList =
        [ ("_" ++ capitalizeFirst (show k), KeywordUnderscore k) | k <- [minBound .. maxBound]
        ]
    keywordExtendedList =
        [ (map toLower (show k), KeywordExtended k) | k <- [minBound .. maxBound]
        ]

identKind :: String -> TokenKind
identKind s = fromMaybe IDENT (Map.lookup s tokenKindMap)

isKw :: (Data a) => a -> Bool
isKw k =
    toConstr k
        `elem` map toConstr [minBound .. maxBound :: Keyword]
        || toConstr k
        `elem` map toConstr [minBound .. maxBound :: KeywordUnderscore]
        || toConstr k
        `elem` map toConstr [minBound .. maxBound :: KeywordExtended]

data Constant
    = DECIMAL
    | OCTAL
    | HEX
    | DECIMAL_FLOAT
    | HEX_FLOAT
    | CHAR_LIT
    | STRING_LIT
    deriving
        ( Eq
        , Show
        , Enum
        , Bounded
        , Data
        , Typeable
        )

isConstant :: (Data a) => a -> Bool
isConstant c =
    toConstr c
        `elem` map toConstr [minBound .. maxBound :: Constant]

data Punctuation
    = LBRACK
    | RBRACK
    | LPAREN
    | RPAREN
    | LBRACE
    | RBRACE
    | DOT
    | ARROW
    | PLUS_PLUS
    | MINUS_MINUS
    | AMP
    | STAR
    | PLUS
    | MINUS
    | TILDE
    | BANG
    | SLASH
    | PERCENT
    | LESS_LESS
    | GREATER_GREATER
    | LESS
    | GREATER
    | LESS_EQUAL
    | GREATER_EQUAL
    | EQUAL_EQUAL
    | BANG_EQUAL
    | CARET
    | PIPE
    | AMP_AMP
    | PIPE_PIPE
    | QUESTION
    | COLON
    | SEMI
    | ELLIPSIS
    | EQUAL
    | STAR_EQUAL
    | SLASH_EQUAL
    | PERCENT_EQUAL
    | PLUS_EQUAL
    | MINUS_EQUAL
    | LESS_LESS_EQUAL
    | GREATER_GREATER_EQUAL
    | AMP_EQUAL
    | CARET_EQUAL
    | PIPE_EQUAL
    | COMMA
    | LESS_COLON
    | COLON_GREATER
    | LESS_PERCENT
    | PERCENT_GREATER
    deriving
        ( Eq
        , Show
        , Enum
        , Bounded
        , Data
        , Typeable
        )

isPunctuation :: (Data a) => a -> Bool
isPunctuation p =
    toConstr p
        `elem` map toConstr [minBound .. maxBound :: Punctuation]

punctuators :: Map String Punctuation
punctuators =
    Map.fromList
        [ ("[", LBRACK)
        , ("]", RBRACK)
        , ("(", LPAREN)
        , (")", RPAREN)
        , ("{", LBRACE)
        , ("}", RBRACE)
        , (".", DOT)
        , ("->", ARROW)
        , ("++", PLUS_PLUS)
        , ("--", MINUS_MINUS)
        , ("&", AMP)
        , ("*", STAR)
        , ("+", PLUS)
        , ("-", MINUS)
        , ("~", TILDE)
        , ("!", BANG)
        , ("/", SLASH)
        , ("%", PERCENT)
        , ("<<", LESS_LESS)
        , (">>", GREATER_GREATER)
        , ("<", LESS)
        , (">", GREATER)
        , ("<=", LESS_EQUAL)
        , (">=", GREATER_EQUAL)
        , ("==", EQUAL_EQUAL)
        , ("!=", BANG_EQUAL)
        , ("^", CARET)
        , ("|", PIPE)
        , ("&&", AMP_AMP)
        , ("||", PIPE_PIPE)
        , ("?", QUESTION)
        , (":", COLON)
        , (";", SEMI)
        , ("...", ELLIPSIS)
        , ("=", EQUAL)
        , ("*=", STAR_EQUAL)
        , ("/=", SLASH_EQUAL)
        , ("%=", PERCENT_EQUAL)
        , ("+=", PLUS_EQUAL)
        , ("-=", MINUS_EQUAL)
        , ("<<=", LESS_LESS_EQUAL)
        , (">>=", GREATER_GREATER_EQUAL)
        , ("&=", AMP_EQUAL)
        , ("^=", CARET_EQUAL)
        , ("|=", PIPE_EQUAL)
        , (",", COMMA)
        , ("<:", LESS_COLON)
        , (":>", COLON_GREATER)
        , ("<%", LESS_PERCENT)
        , ("%>", PERCENT_GREATER)
        ]

isPunctuatorStart :: Char -> Bool
isPunctuatorStart c = c `elem` map head (Map.keys punctuators)

punctuatorsNext :: Map Char [String]
punctuatorsNext = Map.map (sortOn (negate . length)) initialMap
  where
    initialMap = Map.fromListWith (++) [(head k, [k]) | k <- Map.keys punctuators]

resolveDigraph :: Punctuation -> Punctuation
resolveDigraph LESS_COLON = LBRACK
resolveDigraph COLON_GREATER = RBRACK
resolveDigraph LESS_PERCENT = LBRACE
resolveDigraph PERCENT_GREATER = RBRACE
resolveDigraph tk = tk

data Token = Token {tokKind :: TokenKind, tokPos :: Int, tokText :: String}

instance Show Token where
    show (Token k _ t) = show k ++ " (" ++ show t ++ ")"
