module Repl
  ( evalAndPrint,
    runOne,
    runRepl,
  )
where

import Eval (eval)
import Parse (readExpr)
import Primitives (primitiveBindings)
import State (liftThrows, runIOThrows)
import System.IO (hFlush, stdout)
import Val (Env)

-- Evaluate a single expression and print the result.
runOne :: String -> IO ()
runOne expr = primitiveBindings >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl = do
  env <- primitiveBindings
  until_ (== "quit") (readPrompt "Lithp>>> ") (evalAndPrint env)

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

-- Prints out a string and immediately flushes the stream; otherwise, output
-- might sit in output buffers and the user will never see prompts or results.
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ fmap show $ liftThrows (readExpr expr) >>= eval env

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ predicate prompt action = do
  result <- prompt
  if predicate result
    then return ()
    else action result >> until_ predicate prompt action
