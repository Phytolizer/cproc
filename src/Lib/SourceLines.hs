module Lib.SourceLines (
    SourcePos (..),
    initSourceLines,
    posFromOffset,
) where

import qualified Data.Text as T
import Prelude hiding (lines)

data SourcePos = SourcePos
    { posFilename :: FilePath
    , posRow :: Int
    , posCol :: Int
    }

instance Show SourcePos where
    show (SourcePos fp row col) = fp ++ ":" ++ show row ++ ":" ++ show col

data SourceLines = SourceLines
    { linesFilename :: FilePath
    , linesStarts :: [Int]
    , linesFileLength :: Int
    , linesLast :: Int
    }

findIndices :: (Char -> Bool) -> T.Text -> [Int]
findIndices p = go 0
  where
    go i t
        | T.null t = []
        | p (T.head t) = i : go (i + 1) (T.tail t)
        | otherwise = go (i + 1) (T.tail t)

elemIndices :: Char -> T.Text -> [Int]
elemIndices = findIndices . (==)

initSourceLines :: FilePath -> T.Text -> SourceLines
initSourceLines fp source = SourceLines fp starts (T.length source) (length starts + 1)
  where
    starts = 0 : map (+ 1) (elemIndices '\n' source)

countWhile :: (a -> Bool) -> [a] -> Int
countWhile p = length . takeWhile p

posFromOffset :: SourceLines -> Int -> SourcePos
posFromOffset lines offset =
    if offset >= linesFileLength lines
        then SourcePos (linesFilename lines) (linesLast lines) 1
        else SourcePos (linesFilename lines) row (col + 1)
  where
    row = countWhile (<= offset) (linesStarts lines)
    col = offset - (linesStarts lines !! (row - 1))
