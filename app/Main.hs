module Main (main) where

import Eval (eval)
import Parse (readExpr)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= print . eval . readExpr . head -- TODO: partial!
