module Main (main) where

import Lib (readExpr)
import System.Environment (getArgs)

main :: IO ()
main = do
  (expr : _) <- getArgs
  putStrLn (readExpr expr)
