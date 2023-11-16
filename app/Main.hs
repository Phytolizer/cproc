module Main (main) where

import Lib.Process (processFiles)
import System.Environment (getArgs)

main :: IO ()
main = processFiles =<< getArgs
