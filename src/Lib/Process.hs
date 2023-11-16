module Lib.Process (processFiles) where

import Lib.Lexer (initLexer, intoTokens)
import Lib.SourceLines (initSourceLines, posFromOffset)

import qualified Data.Text.IO as TIO
import Lib.Token (Token (tokPos))
import Prelude hiding (lines)

processFile :: FilePath -> IO ()
processFile fp = do
    rawSource <- TIO.readFile fp
    let lines = initSourceLines fp rawSource
    mapM_
        (\tok -> putStrLn $ show (posFromOffset lines (tokPos tok)) ++ ": " ++ show tok)
        . intoTokens
        $ initLexer rawSource

processFiles :: [FilePath] -> IO ()
processFiles = mapM_ processFile
