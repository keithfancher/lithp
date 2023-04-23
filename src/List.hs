module List
  ( car,
    cdr,
    cons,
  )
where

import Control.Monad.Except (throwError)
import Error (LispError (..), ThrowsError)
import Val (LispVal (..))

car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)] = return x
car [DottedList (x : _) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
-- Dotted list should stay dotted:
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
-- I don't quite get this one. "If you cons together two non-lists, or put a
-- list in front, you get a DottedList. This is because such a cons cell isn't
-- terminated by the normal Nil that most lists are."
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList
