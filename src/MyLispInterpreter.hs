module MyLispInterpreter where

import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans
import Data.IORef
import System.IO
import System.Exit

import MyLispCommon
import MyLispParser

eval :: Environment -> Expression -> ExecutionIO Expression
eval envRef bool@(Bool _) = return bool
eval envRef int@(Integer _) = return int
eval envRef str@(String _) = return str
eval envRef (Symbol sym) = getSymbol envRef sym
eval envRef (List [Symbol "quote", exp]) = return exp
eval envRef (List (Symbol "load" : [String arg])) =
    loadSourceFile arg >>= \exps -> liftM last (mapM (eval envRef) exps)
eval envRef (List (Symbol "begin" : exps)) =
    liftM last (mapM (eval envRef) exps)
eval envRef (List [Symbol "if", expCond, expThen, expElse]) = do
    condVal <- eval envRef expCond
    case condVal of
        Bool True -> eval envRef expThen
        Bool False -> eval envRef expElse
        notBool -> throwError $ TypeError "boolean" notBool
eval envRef (List [Symbol "set!", Symbol sym, exp]) =
    eval envRef exp >>= setSymbol envRef sym
eval envRef (List [Symbol "define", Symbol sym, exp]) =
    eval envRef exp >>= defineSymbol envRef sym
eval envRef (List (Symbol "define" : List (Symbol sym : params) : body)) =
    createNormalFunction envRef params body >>= defineSymbol envRef sym
eval envRef (List (Symbol "define" :
    DottedList (Symbol sym : params) varArg : body)) =
        createVarArgFunction varArg envRef params body
        >>= defineSymbol envRef sym
eval envRef (List (Symbol "lambda" : List params : body)) =
    createNormalFunction envRef params body
eval envRef (List (Symbol "lambda" : DottedList params varArg : body)) =
    createVarArgFunction varArg envRef params body
eval envRef (List (Symbol "lambda" : varArg@(Symbol _) : body)) =
    createVarArgFunction varArg envRef [] body
eval envRef (List (sym : args)) = do
    fun <- eval envRef sym
    argVals <- mapM (eval envRef) args
    apply fun argVals
eval _ badForm = throwError $ BadFormError "unrecognized expression" badForm

loadSourceFile :: String -> ExecutionIO [Expression]
loadSourceFile filename =
    liftIO (catch (readFile filename)
    (\_ -> return $ "(error \"file '" ++ filename ++ "' inaccessible\")")) >>=
    liftExecution . lispManyParser

getSymbol :: Environment -> String -> ExecutionIO Expression
getSymbol envRef sym = do
    env <- liftIO (readIORef envRef)
    case (lookup sym env) of
        Nothing ->
            throwError $ UnboundSymbolError "getting symbol not bounded" sym
        Just symRef -> liftIO (readIORef symRef)

setSymbol :: Environment -> String -> Expression -> ExecutionIO Expression
setSymbol envRef sym exp = do
    env <- liftIO (readIORef envRef)
    case (lookup sym env) of
        Nothing ->
            throwError $ UnboundSymbolError "setting symbol not bounded" sym
        Just symRef -> liftIO (writeIORef symRef exp)
    return exp

isSymbolBound :: Environment -> String -> IO Bool
isSymbolBound envRef sym = do
    env <- readIORef envRef
    case (lookup sym env) of
        Nothing -> return False
        Just _ -> return True

defineSymbol :: Environment -> String -> Expression -> ExecutionIO Expression
defineSymbol envRef sym exp = do
    alreadyDefined <- liftIO (isSymbolBound envRef sym)
    if alreadyDefined
        then setSymbol envRef sym exp
        else liftIO $ do
            expRef <- newIORef exp
            env <- readIORef envRef
            writeIORef envRef ((sym, expRef) : env)
            return exp

bindSymbol :: (String, Expression) -> IO (String, IORef Expression)
bindSymbol (sym, exp) = do { expRef <- newIORef exp; return (sym, expRef) }

bindSymbols :: Environment -> [(String, Expression)] -> IO Environment
bindSymbols envRef symExpList = readIORef envRef >>=
    (\env -> liftM (++ env) (mapM bindSymbol symExpList)) >>= newIORef

createFunction varArg envRef params body =
    return $ UserDefinedFunction (map show params) varArg body envRef

createNormalFunction = createFunction Nothing

createVarArgFunction = createFunction . Just . show

apply :: Expression -> [Expression] -> ExecutionIO Expression
apply (BuiltinFunction fun) args = liftExecution (fun args)
apply (BuiltinIOFunction fun) args = fun args
apply (UserDefinedFunction params varArg body envRef) args =
    if numParams /= numArgs && varArg == Nothing
        then throwError $ NumArgsError numParams args
        else liftIO (bindSymbols envRef (zip params args)) >>=
            bindVarArg varArg >>= evalFunctionBody
    where
        numParams = toInteger (length params)
        numArgs = toInteger (length args)
        evalFunctionBody envRef = liftM last (mapM (eval envRef) body)
        bindVarArg varArg env = case varArg of
            Just arg -> liftIO (bindSymbols env
                [(arg, List $ drop (length params) args)])
            Nothing -> return env
apply badForm _ = throwError $ NotFunctionError "not a function" (show badForm)

builtinFunctions :: [(String, [Expression] -> Execution Expression)]
builtinFunctions = [("+", builtinIntBinOp (+)),
                    ("-", builtinIntBinOp (-)),
                    ("*", builtinIntBinOp (*)),
                    ("/", builtinIntBinOp div),
                    ("quotient", builtinIntBinOp quot),
                    ("modulo", builtinIntBinOp mod),
                    ("remainder", builtinIntBinOp rem),
                    ("=", builtinIntBoolBinOp (==)),
                    ("<", builtinIntBoolBinOp (<)),
                    (">", builtinIntBoolBinOp (>)),
                    ("<=", builtinIntBoolBinOp (<=)),
                    (">=", builtinIntBoolBinOp (>=)),
                    ("/=", builtinIntBoolBinOp (/=)),
                    ("&&", builtinBoolBoolBinOp (&&)),
                    ("||", builtinBoolBoolBinOp (||)),
                    ("string=?", builtinStrBoolBinOp (==)),
                    ("string<?", builtinStrBoolBinOp (<)),
                    ("string>?", builtinStrBoolBinOp (>)),
                    ("string<=?", builtinStrBoolBinOp (<=)),
                    ("string>=", builtinStrBoolBinOp (>=)),
                    ("string/=", builtinStrBoolBinOp (/=)),
                    ("boolean?", builtinIsBoolean),
                    ("integer?", builtinIsInteger),
                    ("string?", builtinIsString),
                    ("list?", builtinIsList),
                    ("dotted-list?", builtinIsDottedList),
                    ("symbol?", builtinIsSymbol),
                    ("eqv?", builtinEqv),
                    ("car", builtinCar),
                    ("cdr", builtinCdr),
                    ("cons", builtinCons)]

builtinIntBinOp :: (Integer -> Integer -> Integer) -> [Expression] ->
    Execution Expression
builtinIntBinOp op arg@[_] = throwError $ NumArgsError 2 arg
builtinIntBinOp op args = do
    unpackedArgs <- mapM unpackInt args
    return $ Integer (foldl1 op unpackedArgs)

boolBinOp :: (Expression -> Execution a) -> (a -> a -> Bool) -> [Expression] ->
    Execution Expression
boolBinOp unpacker op [arg1, arg2] = do
    left <- unpacker arg1
    right <- unpacker arg2
    return $ Bool (left `op` right)
boolBinOp _ _ args = throwError $ NumArgsError 2 args

builtinIntBoolBinOp = boolBinOp unpackInt

unpackInt :: Expression -> Execution Integer
unpackInt (Integer v) = return v
unpackInt arg = throwError $ TypeError "integer" arg

builtinBoolBoolBinOp = boolBinOp unpackBool

unpackBool :: Expression -> Execution Bool
unpackBool (Bool v) = return v
unpackBool arg = throwError $ TypeError "boolean" arg

builtinStrBoolBinOp = boolBinOp unpackStr

unpackStr :: Expression -> Execution String
unpackStr (String v) = return v
unpackStr arg = throwError $ TypeError "string" arg

builtinIsBoolean :: [Expression] -> Execution Expression
builtinIsBoolean [(Bool _)] = return $ Bool True
builtinIsBoolean [_] = return $ Bool False
builtinIsBoolean args = throwError $ NumArgsError 1 args

builtinIsInteger :: [Expression] -> Execution Expression
builtinIsInteger [(Integer _)] = return $ Bool True
builtinIsInteger [_] = return $ Bool False
builtinIsInteger args = throwError $ NumArgsError 1 args

builtinIsString :: [Expression] -> Execution Expression
builtinIsString [(String _)] = return $ Bool True
builtinIsString [_] = return $ Bool False
builtinIsString args = throwError $ NumArgsError 1 args

builtinIsList :: [Expression] -> Execution Expression
builtinIsList [(List _)] = return $ Bool True
builtinIsList [_] = return $ Bool False
builtinIsList args = throwError $ NumArgsError 1 args

builtinIsDottedList :: [Expression] -> Execution Expression
builtinIsDottedList [(DottedList _ _)] = return $ Bool True
builtinIsDottedList [_] = return $ Bool False
builtinIsDottedList args = throwError $ NumArgsError 1 args

builtinIsSymbol :: [Expression] -> Execution Expression
builtinIsSymbol [(Symbol _)] = return $ Bool True
builtinIsSymbol [_] = return $ Bool False
builtinIsSymbol args = throwError $ NumArgsError 1 args

builtinEqv :: [Expression] -> Execution Expression
builtinEqv [(Symbol arg1), (Symbol arg2)] = return $ Bool (arg1 == arg2)
builtinEqv [(Bool arg1), (Bool arg2)] = return $ Bool (arg1 == arg2)
builtinEqv [(Integer arg1), (Integer arg2)] = return $ Bool (arg1 == arg2)
builtinEqv [(String arg1), (String arg2)] = return $ Bool (arg1 == arg2)
builtinEqv [(List arg1), (List arg2)] = return $
    Bool (length arg1 == length arg2 && (and (map eqvPair (zip arg1 arg2))))
    where eqvPair (x, y) = case builtinEqv [x, y] of
            Left _ -> False
            Right (Bool val) -> val
builtinEqv [(DottedList xs x), (DottedList ys y)] =
    builtinEqv [List $ xs ++ [x], List $ ys ++ [y]]
builtinEqv [_, _] = return $ Bool False
builtinEqv args = throwError $ NumArgsError 2 args

builtinCar :: [Expression] -> Execution Expression
builtinCar [List (h : _)] = return h
builtinCar [DottedList (h : _) _] = return h
builtinCar [arg] = throwError $ TypeError "non-empty list or dotted list" arg
builtinCar args = throwError $ NumArgsError 1 args

builtinCdr :: [Expression] -> Execution Expression
builtinCdr [List (_ : t)] = return $ List t
builtinCdr [DottedList [_] last] = return last
builtinCdr [DottedList (_ : t) last] = return $ DottedList t last
builtinCdr [arg] = throwError $ TypeError "non-empty list or dotted list" arg
builtinCdr args = throwError $ NumArgsError 1 args

builtinCons :: [Expression] -> Execution Expression
builtinCons [x, List []] = return $ List [x]
builtinCons [x, List xs] = return $ List (x : xs)
builtinCons [x, DottedList xs last] = return $ DottedList (x : xs) last
builtinCons [x1, x2] = return $ DottedList [x1] x2
builtinCons args = throwError $ NumArgsError 2 args

builtinIOFunctions :: [(String, [Expression] -> ExecutionIO Expression)]
builtinIOFunctions = [("quit", builtinQuit),
                      ("display", builtinDisplay),
                      ("newline", builtinNewline),
                      ("read", builtinRead),
                      ("apply", builtinApply),
                      ("error", builtinError)]

builtinQuit :: [Expression] -> ExecutionIO Expression
builtinQuit [] = liftIO (exitSuccess >> (return $ Bool True))
builtinQuit [Bool True] = builtinQuit []
builtinQuit _ = liftIO (exitFailure >> (return $ Bool False))

builtinDisplay :: [Expression] -> ExecutionIO Expression
builtinDisplay [String msg] = liftIO (putStr msg >> (return $ Symbol ""))
builtinDisplay [arg] = throwError $ TypeError "string" arg
builtinDisplay args = throwError $ NumArgsError 1 args

builtinNewline :: [Expression] -> ExecutionIO Expression
builtinNewline [] = liftIO (putStrLn "" >> (return $ Symbol ""))
builtinNewline args = throwError $ NumArgsError 0 args

builtinRead :: [Expression] -> ExecutionIO Expression
builtinRead [] = liftIO (hGetLine stdin >>= (return . String))
builtinRead args = throwError $ NumArgsError 0 args

builtinApply :: [Expression] -> ExecutionIO Expression
builtinApply [f, List args] = apply f args
builtinApply (f : args) = apply f args

builtinError :: [Expression] -> ExecutionIO Expression
builtinError [String msg] = throwError $ DefaultError msg
builtinError [arg] = throwError $ TypeError "string" arg
builtinError args = throwError $ NumArgsError 1 args

startEnvironment :: IO Environment
startEnvironment = do
    emptyEnv <- newIORef []
    let builtinFunction cons (sym, fun) = (sym, cons fun)
    builtinEnv <- bindSymbols emptyEnv (map (builtinFunction BuiltinFunction)
        builtinFunctions)
    bindSymbols builtinEnv (map (builtinFunction BuiltinIOFunction)
        builtinIOFunctions)

interpreter :: Environment -> String -> IO String
interpreter envRef input = runExecutionIO (liftM show (liftExecution
    (lispParser input) >>= eval envRef))
