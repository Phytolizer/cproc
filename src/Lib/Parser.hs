module Lib.Parser (

)
where

import Control.Monad.State (State, gets, modify)
import Data.Map (Map)
import qualified Data.Map as Map
import Lib.Token (Token)

type Scope = Map String Bool

data ParserState = ParserState
  { parserTokens :: [Token]
  , parserScopes :: [Scope]
  }

type Parser = State ParserState

initParser :: [Token] -> ParserState
initParser tokens = ParserState tokens []

pushScope :: Parser ()
pushScope = modify $ \s -> s{parserScopes = Map.empty : parserScopes s}

popScope :: Parser ()
popScope = modify $ \s -> s{parserScopes = tail $ parserScopes s}

current :: Parser Token
current = gets (head . parserTokens)

next :: Parser Token
next = do
  tok <- current
  modify $ \s -> s{parserTokens = tail $ parserTokens s}
  return tok

addTypedefName :: String -> Parser ()
addTypedefName name = do
  lastScope <- gets (head . parserScopes)
  case Map.lookup name lastScope of
    Just False -> error $ "typedef name " ++ name ++ " is already used as a non-typedef"
    _ -> modify $ \s -> s{parserScopes = Map.insert name True lastScope : tail (parserScopes s)}

addIdentifier :: String -> Parser ()
addIdentifier name = do
  lastScope <- gets (head . parserScopes)
  case Map.lookup name lastScope of
    Just True -> error $ "identifier " ++ name ++ " is already used as a typedef"
    _ -> modify $ \s -> s{parserScopes = Map.insert name False lastScope : tail (parserScopes s)}

isTypeInScope :: String -> Parser (Maybe Scope)
isTypeInScope name = do
  scopes <- gets parserScopes
  return
    $ foldr
      ( \scope acc -> case Map.lookup name scope of
          Just True -> Just scope
          _ -> acc
      )
      Nothing
      scopes
