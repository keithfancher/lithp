module Repl
  ( evalAndPrint,
    runRepl,
  )
where

import Error (extractValue, trapError)
import Eval (eval)
import Parse (readExpr)
import System.IO

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lithp>>> ") evalAndPrint

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

-- Prints out a string and immediately flushes the stream; otherwise, output
-- might sit in output buffers and the user will never see prompts or results.
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (fmap show $ readExpr expr >>= eval)

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ predicate prompt action = do
  result <- prompt
  if predicate result
    then return ()
    else action result >> until_ predicate prompt action
