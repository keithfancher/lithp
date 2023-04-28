module Main (main) where

import Repl (runOne, runRepl)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runRepl
    [expr] -> runOne expr
    _ -> putStrLn "Program takes only 0 or 1 argument"
