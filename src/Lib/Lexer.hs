module Lib.Lexer (
  Lexer,
  initLexer,
  allTokens,
  nextToken,
  intoTokens,
)
where

import Control.Applicative (many)
import Control.Monad (forM_, guard, mzero, void)
import Control.Monad.Extra (ifM, unlessM)
import Control.Monad.Loops (allM, andM, anyM, orM, takeWhileM, whileM_)
import Control.Monad.State (MonadState (get), StateT, gets, modify, runStateT)
import Data.Char (isAlpha, isAlphaNum, isDigit, isHexDigit, isOctDigit, isSpace)
import Data.Functor (($>))
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, isNothing)
import qualified Data.Text as T
import Lib.Token (
  Constant (..),
  Token (..),
  TokenKind (..),
  identKind,
  isPunctuatorStart,
  punctuators,
  punctuatorsNext,
  resolveDigraph,
 )
import Prelude hiding (exp, pred)

type Lexer = StateT LexerState Maybe

data LexerState = LexerState
  { lxSrc :: T.Text
  , lxPos :: Int
  , lxWasEof :: Bool
  }

initLexer :: T.Text -> LexerState
initLexer src = LexerState{lxSrc = src, lxPos = 0, lxWasEof = False}

move :: Int -> Lexer ()
move n = modify $ \s -> s{lxPos = lxPos s + n}

move1 :: Lexer ()
move1 = move 1

look :: Int -> Lexer (Maybe Char)
look offset = do
  s <- get
  let index = lxPos s + offset
  return
    $ if index < T.length (lxSrc s)
      then Just $ T.index (lxSrc s) index
      else Nothing

lookCh :: Int -> Char -> Lexer Bool
lookCh offset exp = do
  ch <- look offset
  return $ ch == Just exp

lookPred :: Int -> (Char -> Bool) -> Lexer Bool
lookPred offset pred = do
  ch <- look offset
  return $ maybe False pred ch

hasPrefix :: String -> Int -> Lexer Bool
hasPrefix prefix offset = do
  if null prefix
    then move offset >> return True
    else do
      matches <- mapM checkChar (zip [offset ..] prefix)
      return $ and matches
 where
  checkChar (i, ch) = lookCh i ch

hasPrefix' :: String -> Lexer Bool
hasPrefix' prefix = hasPrefix prefix 0

tokenHasPrefix :: String -> Int -> Lexer Bool
tokenHasPrefix prefix start = do
  s <- get
  let index = lxPos s + start
  return
    ( (index + length prefix <= T.length (lxSrc s))
        && T.isPrefixOf (T.pack prefix) (T.drop index (lxSrc s))
    )

matchPrefixOffset :: String -> Int -> Lexer Bool
matchPrefixOffset prefix offset = do
  matches <- hasPrefix prefix offset
  if matches
    then move (offset + length prefix) >> return True
    else return False

matchPrefix :: String -> Lexer Bool
matchPrefix prefix = matchPrefixOffset prefix 0

skipWs :: Lexer ()
skipWs = skipWs' False False
 where
  skipWs' :: Bool -> Bool -> Lexer ()
  skipWs' blockComment lineComment = do
    ifM (isNothing <$> look 0) (return ())
    $ case (blockComment, lineComment) of
      (True, _) ->
        ifM
          (matchPrefix "*/")
          (skipWs' False lineComment)
          (move 1 >> skipWs' True lineComment)
      (False, True) ->
        ifM
          (lookCh 0 '\n')
          (move 1 >> skipWs' blockComment False)
          (move 1 >> skipWs' blockComment True)
      (_, _) -> do
        ifM (lookPred 0 isSpace) (move 1 >> skipWs' blockComment lineComment)
          $ ifM (matchPrefix "/*") (skipWs' True lineComment)
          $ ifM (matchPrefix "//") (skipWs' blockComment True)
          $ return ()

lxIsPunct :: Lexer Bool
lxIsPunct = lookPred 0 isPunctuatorStart

lexPunct :: Lexer TokenKind
lexPunct = do
  start <- fromJust <$> look 0
  let possibilities = fromJust $ Map.lookup start punctuatorsNext
  lexPunct' possibilities
 where
  lexPunct' :: [String] -> Lexer TokenKind
  lexPunct' [] = undefined
  lexPunct' (p : ps) =
    if length p == 1
      then move 1 >> return (Punctuation $ fromJust $ Map.lookup p punctuators)
      else do
        ifM
          (matchPrefixOffset (tail p) 1)
          (return (Punctuation $ resolveDigraph $ fromJust $ Map.lookup p punctuators))
          (lexPunct' ps)

stringPrefixes :: [String]
stringPrefixes = ["\"", "u8\"", "u\"", "L\"", "U\""]

lxIsStringStart :: Lexer Bool
lxIsStringStart = anyM matchPrefix stringPrefixes

charPrefixes :: [String]
charPrefixes = ["'", "u'", "L'", "U'"]

lxIsCharStart :: Lexer Bool
lxIsCharStart = anyM matchPrefix charPrefixes

checkUniversalCharName :: Int -> Lexer Int
checkUniversalCharName offset = do
  first <- fromJust <$> look offset
  maybe (return 0) checkHexDigits $ case first of
    'u' -> Just 4
    'U' -> Just 8
    _ -> Nothing
 where
  checkHexDigits :: Int -> Lexer Int
  checkHexDigits count = do
    hexDigits <- allM (\i -> lookPred (i + offset) isHexDigit) [1 .. count]
    return $ fromMaybe 0 (guard hexDigits $> count)

lexUniversalCharName :: Lexer Bool
lexUniversalCharName = do
  count <- checkUniversalCharName 0
  if count > 0
    then move (1 + count) >> return True
    else return False

escapeChars :: [Char]
escapeChars = ['\'', '"', '?', '\\', 'a', 'b', 'f', 'n', 'r', 't', 'v']

matchEscapeSeq :: Int -> Lexer Bool
matchEscapeSeq codeUnitBytes =
  ifM (anyM matchPrefix (map return escapeChars)) (return True)
    $ ifM (lookPred 0 isOctDigit) checkOctal
    $ ifM (matchPrefix "x") checkHexDigits
    $ ifM (anyM hasPrefix' ["u", "U"]) lexUniversalCharName
    $ return False
 where
  checkOctal = do
    forM_ [0 .. 2] checkOctDigit
    return True
  checkOctDigit i = unlessM (lookPred i isOctDigit) (void (move i))
  checkHexDigits = do
    hexDigits <- takeWhileM (`lookPred` isHexDigit) [0 .. codeUnitBytes * 2 - 1]
    let count = length hexDigits
    return (count <= (codeUnitBytes * 2))

lexString :: Int -> Lexer TokenKind
lexString start = do
  codeUnitBytes <-
    ifM (tokenHasPrefix "U" start) (return 4)
      $ ifM (tokenHasPrefix "u8" start) (return 1)
      $ ifM (tokenHasPrefix "u" start) (return 2)
      $ ifM (tokenHasPrefix "L" start) (error "TODO: find a way to get wchar_t size")
      $ return 1
  loop codeUnitBytes
 where
  loop codeUnitBytes = do
    ifM (hasPrefix' "\n") (return ERROR)
      $ ifM (matchPrefix "\"") (return $ Constant STRING_LIT)
      $ ifM
        (matchPrefix "\\")
        ( ifM
            (matchEscapeSeq codeUnitBytes)
            (loop codeUnitBytes)
            (return ERROR)
        )
      $ move 1
      >> loop codeUnitBytes

lexChar :: Int -> Lexer TokenKind
lexChar start = do
  codeUnitBytes <-
    ifM (tokenHasPrefix "U" start) (return 4)
      $ ifM (tokenHasPrefix "u" start) (return 2)
      $ ifM (tokenHasPrefix "L" start) (error "TODO: find a way to get wchar_t size")
      $ return 1
  loop codeUnitBytes (Constant CHAR_LIT)
 where
  loop codeUnitBytes result = do
    ifM (matchPrefix "\n") (return ERROR)
      $ ifM (matchPrefix "'") (return result)
      $ ifM
        (matchPrefix "\\")
        ( do
            isEscapeSeq <- matchEscapeSeq codeUnitBytes
            loop codeUnitBytes (if isEscapeSeq then result else ERROR)
        )
      $ move 1
      >> loop codeUnitBytes result

isIdentStart :: Lexer Bool
isIdentStart =
  ifM
    (lookCh 0 '\\')
    ((/= 0) <$> checkUniversalCharName 1)
    (lookPred 0 (\c -> isAlpha c || c == '_'))

lexIdent :: Lexer TokenKind
lexIdent =
  do
    ifM (andM [matchPrefix "\\", not <$> lexUniversalCharName]) (return ERROR)
    $ ifM (lookPred 0 (\c -> isAlphaNum c || c == '_')) (move1 >> lexIdent)
    $ return IDENT

isIntStart :: Lexer Bool
isIntStart = lookPred 0 isDigit

lexIntHex :: Lexer Bool
lexIntHex =
  ifM
    (lookPred 0 isHexDigit)
    (move1 >> whileM_ (lookPred 0 isHexDigit) move1 >> return True)
    $ return False

lexIntOctal :: Lexer Bool
lexIntOctal = do
  whileM_ (lookPred 0 isOctDigit) move1
  ifM (lookPred 0 isDigit) (move1 >> return False)
    $ return True

lexIntDecimal :: Lexer Bool
lexIntDecimal =
  ifM (not <$> lookPred 0 isDigit) (return False)
    $ move1
    >> whileM_ (lookPred 0 isDigit) move1
    >> return True

checkIntEnd :: Lexer Bool
checkIntEnd = not <$> orM [isIntStart, lxIsStringStart, lxIsCharStart, isIdentStart]

lexIntSuffix :: Lexer Bool
lexIntSuffix = do
  _ <- anyM andM [matchUAndL, matchLAndU]
  checkIntEnd
 where
  matchU = anyM matchPrefix ["u", "U"]
  matchL = anyM matchPrefix ["ll", "LL", "l", "L"]
  matchUAndL = [matchU, matchL]
  matchLAndU = [matchL, matchU]

lexInt :: Lexer TokenKind
lexInt = do
  kind <- determineIntKind
  ifM lexIntSuffix (return kind) (return ERROR)
 where
  determineIntKind =
    ifM (anyM hasPrefix' ["0x", "0X"]) (move 2 >> ifM lexIntHex (return $ Constant HEX) (return ERROR))
      $ ifM (matchPrefix "0") (ifM lexIntOctal (return $ Constant OCTAL) (return ERROR))
      $ ifM lexIntDecimal (return $ Constant DECIMAL)
      $ return ERROR

nextToken :: Lexer Token
nextToken = ifM (gets lxWasEof) mzero $ do
  skipWs
  start <- gets lxPos
  kind <-
    ifM (isNothing <$> look 0) (modify (\s -> s{lxWasEof = True}) >> return EOF)
      $ ifM lxIsPunct lexPunct
      $ ifM lxIsStringStart (lexString start)
      $ ifM lxIsCharStart (lexChar start)
      $ ifM isIdentStart lexIdent
      $ ifM isIntStart lexInt
      $ move1
      >> return ERROR
  pos <- gets lxPos
  text <-
    gets
      ( T.unpack
          . T.take (pos - start)
          . T.drop start
          . lxSrc
      )
  let kind' = if kind == IDENT then identKind text else kind
  return Token{tokKind = kind', tokPos = start, tokText = text}

allTokens :: Lexer [Token]
allTokens = many nextToken

intoTokens :: LexerState -> [Token]
intoTokens s = fst $ fromJust $ runStateT allTokens s
