module Primitives (primitiveBindings) where

import Control.Monad.Except (catchError, throwError)
import Eval (applyProc)
import IO (closePort, makePort, readAll, readContents, readProc, writeProc)
import List (car, cdr, cons)
import State (bindVars)
import System.IO (IOMode (..))
import Val (Env, IOThrowsError, LispError (..), LispVal (..), ThrowsError, nullEnv)

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= flip bindVars combinedPrimitives
  where
    combinedPrimitives = map (makeFunc IOFunc) ioPrimitives ++ map (makeFunc PrimitiveFunc) primitives
    makeFunc constructor (var, func) = (var, constructor func)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem),
    ("=", numBoolBinop (==)),
    ("<", numBoolBinop (<)),
    (">", numBoolBinop (>)),
    ("/=", numBoolBinop (/=)),
    (">=", numBoolBinop (>=)),
    ("<=", numBoolBinop (<=)),
    ("&&", boolBoolBinop (&&)),
    ("||", boolBoolBinop (||)),
    ("string=?", strBoolBinop (==)),
    ("string<?", strBoolBinop (<)),
    ("string>?", strBoolBinop (>)),
    ("string<=?", strBoolBinop (<=)),
    ("string>=?", strBoolBinop (>=)),
    ("car", car),
    ("cdr", cdr),
    ("cons", cons),
    ("eq?", equalsStrictWrap),
    ("eqv?", equalsStrictWrap),
    ("equal?", equalsLoose)
  ]

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives =
  [ ("apply", applyProc),
    ("open-input-file", makePort ReadMode),
    ("open-output-file", makePort WriteMode),
    ("close-input-port", closePort),
    ("close-output-port", closePort),
    ("read", readProc),
    ("write", writeProc),
    ("read-contents", readContents),
    ("read-all", readAll)
  ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ [] = throwError $ NumArgs 2 []
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = foldInts <$> mapM unpackNum params
  where
    foldInts = Number . foldl1 op -- fold ints into a `LispVal` with `op`

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

-- Generic helper to run binary operations on various input types.
boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op [arg1, arg2] = do
  left <- unpacker arg1
  right <- unpacker arg2
  return $ Bool $ left `op` right
boolBinop _ _ notTwoArgs = throwError $ NumArgs 2 notTwoArgs

-- Weak typing! The following values are essentially equivalent:
--   3, "3", [3], "3 hey there mister"
-- Why? Blame: https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Evaluation,_Part_1
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = weakNumFromString n
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

-- In this conversion, if any string *starts* with an integer, that integer
-- will be successfully parsed out.
weakNumFromString :: String -> ThrowsError Integer
weakNumFromString n = case reads n of
  [] -> throwError $ TypeMismatch "number" $ String n
  (intVal, _) : _ -> return intVal -- `reads` returns a list of pairs, [(a, String)] -- we want the first

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

-- Wrap the `Bool` result of `eqv` into a `LispVal`. (Confusingly, also called
-- `Bool`. Should probably change that.)
equalsStrictWrap :: [LispVal] -> ThrowsError LispVal
equalsStrictWrap args = Bool <$> equalsStrict args

-- Strict(er) equivalence check.
-- See: http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.1
equalsStrict :: [LispVal] -> ThrowsError Bool
equalsStrict [Bool arg1, Bool arg2] = return $ arg1 == arg2
equalsStrict [Number arg1, Number arg2] = return $ arg1 == arg2
equalsStrict [String arg1, String arg2] = return $ arg1 == arg2
equalsStrict [Atom arg1, Atom arg2] = return $ arg1 == arg2
equalsStrict [DottedList xs x, DottedList ys y] = equalsStrict [List $ xs ++ [x], List $ ys ++ [y]]
equalsStrict [List arg1, List arg2] = return $ listsEqualStrict arg1 arg2
equalsStrict [_, _] = return False
equalsStrict badArgList = throwError $ NumArgs 2 badArgList

-- Check for equality of two lists of `LipsVal`s: they're the same length, and
-- each corresponding item is also (strictly) equivalent.
listsEqualStrict :: [LispVal] -> [LispVal] -> Bool
listsEqualStrict list1 list2 =
  (length list1 == length list2)
    && all eqvPair (zip list1 list2)
  where
    eqvPair (x1, x2) = case equalsStrict [x1, x2] of
      Left _ -> False
      Right val -> val

-- We use this for our heterogeneous list of unpackers below:
data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

-- Helper to check equivalence of two args with a given unpacker function.
unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do
    unpacked1 <- unpacker arg1
    unpacked2 <- unpacker arg2
    return $ unpacked1 == unpacked2
    `catchError` const (return False)

-- Attempts to use each of the "unpacker" functions to compare two arguments.
-- (These unpackers are very fast-and-loose with types.) If *any* of these
-- returns `True`, so does this function.
weakTypeEquals :: LispVal -> LispVal -> ThrowsError Bool
weakTypeEquals arg1 arg2 = or <$> mapM (unpackEquals arg1 arg2) allUnpackers
  where
    allUnpackers = [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]

-- Loose(r) equivalence check. Tries both loose and un-loose checks.
equalsLoose :: [LispVal] -> ThrowsError LispVal
equalsLoose [arg1, arg2] = do
  loose <- weakTypeEquals arg1 arg2
  strict <- equalsStrict [arg1, arg2]
  return $ Bool (loose || strict)
equalsLoose badArgList = throwError $ NumArgs 2 badArgList
