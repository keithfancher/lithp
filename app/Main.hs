module Main (main) where

import Repl (runOne, runRepl)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    1 -> runOne $ head args -- TODO: partial!
    _ -> putStrLn "Program takes only 0 or 1 argument"
