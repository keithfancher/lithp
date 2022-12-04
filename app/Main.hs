module Main (main) where

import Parse (readExpr)
import System.Environment (getArgs)

main :: IO ()
main = do
  (expr : _) <- getArgs
  putStrLn (readExpr expr)
