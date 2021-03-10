module Main where

import System.Environment
import Parser

main :: IO ()
main = do
  [expression] <- getArgs
  putStrLn (readExpression expression)
