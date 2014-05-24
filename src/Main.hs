module Main where

import Parser
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    print $ parseString $ head args
