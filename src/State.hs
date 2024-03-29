module State
  ( bindVars,
    defineVar,
    getVar,
    liftThrows,
    runIOThrows,
    setVar,
  )
where

import Control.Monad.Except (liftIO, runExceptT, throwError)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import Val (Env, IOThrowsError, LispError (..), LispVal, ThrowsError, extractValue, trapError)

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . isJust . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Getting an unbound variable" var)
    (liftIO . readIORef)
    (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Setting an unbound variable" var)
    (liftIO . flip writeIORef value)
    (lookup var env)
  return value

-- Handle the special behavior of `define`, which sets a variable if already
-- bound or creates a new one if not.
-- See: https://conservatory.scheme.org/schemers/Documents/Standards/R5RS/HTML/r5rs-Z-H-8.html#%_sec_5.2
defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return value

-- Bind many variables at once.
bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where
    extendEnv bindings' env = fmap (++ env) (mapM addBinding bindings')
    addBinding (var, value) = do
      ref <- newIORef value
      return (var, ref)
