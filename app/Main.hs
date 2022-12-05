module Main (main) where

import Error (extractValue, trapError)
import Eval (eval)
import Parse (readExpr)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let evaled = fmap show $ readExpr (head args) >>= eval -- TODO: partial!
  putStrLn $ extractValue $ trapError evaled
