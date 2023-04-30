module IO
  ( closePort,
    load,
    makePort,
    readAll,
    readContents,
    readProc,
    writeProc,
  )
where

import Control.Monad.Except (liftIO)
import Parse (readExpr, readExprList)
import State (liftThrows)
import System.IO (IOMode, hClose, hGetLine, hPrint, openFile, stdin, stdout)
import Val (IOThrowsError, LispVal (..))

-- TODO: Many of the functions in this file are partial :(

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = fmap Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> return (Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> return (Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = fmap String $ liftIO $ readFile filename

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = List <$> load filename

load :: String -> IOThrowsError [LispVal]
load filename = liftIO (readFile filename) >>= liftThrows . readExprList
