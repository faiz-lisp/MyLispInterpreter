module MyLispInterpreter (getMyLispInterpreter) where

import Control.Monad.Error

import MyLispCommon
import MyLispParser
import MyLispInterpreterCore
import MyLispInterpreterBuiltins
import MyLispUtils

myLispInterpreter :: Environment -> String -> String -> IO String
myLispInterpreter envRef inSrc input = do
    e <- runErrorT (runE (myLispParser inSrc input >>= mapM (eval envRef)))
    case e of
        Left l -> return $ show l
        Right r -> return $ unlines' $ [x | x <- (map show r), (not . null) x]

getMyLispInterpreter :: String -> IO (String -> IO String)
getMyLispInterpreter inSrc = do
    envRef <- createBuiltinEnv builtinFunctions
    return $ myLispInterpreter envRef inSrc