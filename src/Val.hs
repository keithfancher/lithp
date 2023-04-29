module Val
  ( Env,
    IOThrowsError,
    LispVal (..),
    LispFunc (..),
    LispError (..),
    ThrowsError,
    extractValue,
    makeFunc,
    makeNormalFunc,
    makeVarArgs,
    nullEnv,
    trapError,
    unwordsList,
  )
where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (ExceptT, catchError)
import Data.IORef (IORef, newIORef)
import Text.Parsec (ParseError)

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
  | Func LispFunc

-- A user-defined function.
data LispFunc = LispFunc
  { -- the names of the parameters, as they're bound in the function body
    params :: [String],
    -- whether the function accepts a variable-length list of arguments, and if
    -- so, the variable name it's bound to
    vararg :: Maybe String,
    -- the function body, as a list of expressions
    body :: [LispVal],
    -- the environment that the function was created in
    closure :: Env
  }

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList h t) = "(" ++ unwordsList h ++ " . " ++ showVal t ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func LispFunc {params = args, vararg = varargs, body = _, closure = _}) =
  "(lambda ("
    ++ unwords (map show args)
    ++ ( case varargs of
           Nothing -> ""
           Just arg -> " . " ++ arg
       )
    ++ ") ...)"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

type IOThrowsError = ExceptT LispError IO

makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc varargs env params body = return $ Func $ LispFunc (map showVal params) varargs body env

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgs :: LispVal -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarArgs = makeFunc . Just . showVal

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

instance Show LispError where show = showError

type ThrowsError = Either LispError

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

-- TODO: partial. And why does this need to exist?
extractValue :: ThrowsError a -> a
extractValue (Right val) = val

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) =
  "Expected "
    ++ show expected
    ++ " args; found values "
    ++ unwordsList found
showError (TypeMismatch expected found) =
  "Invalid type: expected "
    ++ expected
    ++ ", found "
    ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (Default err) = "Error: " ++ err
