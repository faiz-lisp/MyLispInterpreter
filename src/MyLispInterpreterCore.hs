module MyLispInterpreterCore (createBuiltinEnv,
                              apply,
                              eval) where

import Control.Monad.Error
import Data.IORef
import Data.List
import System.Directory
import System.IO
import System.IO.Error

import MyLispCommon
import MyLispParser
import MyLispUtils

reservedSymbols = ["begin",
                   "cond",
                   "def-list", -- TODO transform this in a normal procedure
                   "define",
                   "else",
                   "if",
                   "lambda",
                   "let",
                   "load",
                   "quote",
                   "set!"]

bindSymbols :: Environment -> [(String, Expression)] -> IO Environment
bindSymbols envRef symExpLst = do
    env <- readIORef envRef
    let bind (sym, exp) = newIORef exp >>= return . (\expRef -> (sym, expRef))
    liftM (++ env) (mapM bind symExpLst) >>= newIORef

getSymbol :: Environment -> String -> Execution Expression
getSymbol envRef sym = do
    env <- liftE $ readIORef envRef
    case lookup sym env of
        Nothing -> throwError $ UnboundSymbolError "getting unbound symbol" sym
        Just expRef -> liftE $ readIORef expRef

evalDefineSF :: Environment -> String -> Expression -> Execution Expression
evalDefineSF envRef sym exp
    | sym `elem` reservedSymbols = throwError $
        BadFormError "can't override reserved symbol" (Symbol sym)
    | otherwise = do
        env <- liftE $ readIORef envRef
        liftE $ case lookup sym env of
            Nothing -> newIORef exp >>=
                (\expRef -> writeIORef envRef ((sym, expRef) : env))
            Just expRef -> writeIORef expRef exp
        return Void

evalSetSF :: Environment -> String -> Expression -> Execution Expression
evalSetSF envRef sym exp
    | sym `elem` reservedSymbols = throwError $
        BadFormError "can't override reserved symbol" (Symbol sym)
    | otherwise = do
        env <- liftE $ readIORef envRef
        case lookup sym env of
            Nothing -> throwError $
                UnboundSymbolError "setting unbound symbol" sym
            Just expRef -> liftE $ writeIORef expRef exp
        return exp

evalLambdaSF varArg params body envRef def
    | not (checkParams params []) = throwError $
        BadFormError "invalid parameter list" def
    | null body = throwError $ BadFormError "empty body" def
    | otherwise = return $
        UserDefinedFunction (map show params) varArg body envRef
    where
        checkParams [] _ = True
        checkParams (exp@(Symbol p) : ps) checkedParams
            | p `elem` checkedParams = False
            | otherwise = checkParams ps (p:checkedParams)
        checkParams (p:_) _ = False

evalLambdaSFNormalArg = evalLambdaSF Nothing

evalLambdaSFVarArg (Symbol varArgSym) params body envRef def =
    evalLambdaSF (Just varArgSym) params body envRef def
evalLambdaSFVarArg _ _ _ _ def =
    throwError $ BadFormError "invalid parameter list" def

evalCondSF :: Environment -> [Expression] -> Execution Expression
evalCondSF _ [] = return $ Bool False
evalCondSF envRef [badForm@(List (Symbol "else" : elseExps))]
    | null elseExps = throwError $ BadFormError ("missing expression in " ++
        "else clause") badForm
    | otherwise = liftM last $ mapM (eval envRef) elseExps
evalCondSF envRef (badForm@(List (Symbol "else" : _)) : _) =
    throwError $ BadFormError "else must be the last cond clause" badForm
evalCondSF envRef ((List (condExp : thenExps)) : exps) = do
    condVal <- eval envRef condExp
    case condVal of
        Bool False -> evalCondSF envRef exps
        _ -> if null thenExps
                then return condVal
                else liftM last $ mapM (eval envRef) thenExps
evalCondSF _ (badForm:_) = throwError $ BadFormError ("cond clause is " ++
    "not a test-value pair") badForm

evalLetSF :: Environment -> [Expression] -> [Expression] -> Execution Expression
evalLetSF envRef bindings body = do
    (formals, args) <- extract bindings [] []
    let lambdaFun = List (Symbol "lambda" : List formals : body)
    eval envRef (List (lambdaFun : args))
    where
        extract [] formals args = return (formals, args)
        extract (List [exp@(Symbol _), arg] : b) formals args =
            extract b (exp:formals) (arg:args)
        extract (badForm:_) _ _ = throwError $ BadFormError ("not " ++
            "a valid binding") badForm

evalLoadSF :: Environment -> String -> Execution Expression
evalLoadSF envRef modPath = do
    f <- liftE $ try $ openFile modPath ReadMode
    case f of
        Left l -> throwError $ IOError $ "\"" ++ modPath ++ "\" (" ++
            ioeGetErrorString l ++ ")"
        Right r -> do
            c <- liftE $ hGetContents r
            finallyError
                (myLispParser modPath c >>=
                    (\exps -> liftM last $ mapM (eval envRef) exps))
                (liftE $ hClose r)

evalDeflistSF :: Environment -> Execution Expression
evalDeflistSF envRef = do
    env <- liftE $ readIORef envRef
    return $ List $ map Symbol $ sort $ map fst env

apply :: Expression -> [Expression] -> Execution Expression
apply (BuiltinFunction fun) args = fun args
apply f@(UserDefinedFunction params varArg body envRef) args
    | (numParams > numArgs) || (numParams < numArgs && varArg == Nothing) =
        throwError $ NumArgsError (show f) numParams args
    | otherwise = liftE (bindSymbols envRef (zip params args)) >>=
        bindVarArg varArg >>= evalFunctionBody
    where
        numParams = length params
        numArgs = length args
        bindVarArg varArg envRef = liftE $ case varArg of
            Just arg -> bindSymbols envRef [(arg, List $ drop numParams args)]
            Nothing -> return envRef
        evalFunctionBody envRef = liftM last $ mapM (eval envRef) body
apply badForm _ = throwError $ BadFormError "not a function" badForm

eval :: Environment -> Expression -> Execution Expression
eval _ exp@(Void) = return exp
eval _ exp@(Bool _) = return exp
eval _ exp@(Integer _) = return exp
eval _ exp@(Real _) = return exp
eval _ exp@(Char _) = return exp
eval _ exp@(String _) = return exp
eval _ (List [Symbol "quote", exp]) = return exp
eval envRef exp@(Symbol sym)
    | sym `elem` reservedSymbols =
        throwError $ BadFormError "invalid use of reserved symbol" exp
    | otherwise = getSymbol envRef sym
eval envRef (List [Symbol "define", Symbol sym, exp]) =
    eval envRef exp >>= evalDefineSF envRef sym
eval envRef exp@(List (Symbol "define" : List (Symbol sym : params) : body)) =
    evalLambdaSFNormalArg params body envRef exp >>= evalDefineSF envRef sym
eval envRef exp@(List (Symbol "define" :
    DottedList (Symbol sym : params) varArg : body)) =
        evalLambdaSFVarArg varArg params body envRef exp >>=
        evalDefineSF envRef sym
eval envRef exp@(List (Symbol "lambda" : List params : body)) =
    evalLambdaSFNormalArg params body envRef exp
eval envRef exp@(List (Symbol "lambda" : Symbol param : body)) =
    evalLambdaSFVarArg (Symbol param) [] body envRef exp
eval envRef exp@(List (Symbol "lambda" : DottedList params varArg : body)) =
    evalLambdaSFVarArg varArg params body envRef exp
eval envRef (List [Symbol "set!", Symbol sym, exp]) =
    eval envRef exp >>= evalSetSF envRef sym
eval envRef (List [Symbol "if", condExp, thenExp]) =
    eval envRef $ List [Symbol "if", condExp, thenExp, Bool False]
eval envRef (List [Symbol "if", condExp, thenExp, elseExp]) = do
    condVal <- eval envRef condExp
    case condVal of
        Bool False -> eval envRef elseExp
        _ -> eval envRef thenExp
eval envRef (List (Symbol "cond" : exps)) = evalCondSF envRef exps
eval envRef (List (Symbol "let" : List bindings : body)) =
    evalLetSF envRef bindings body
eval envRef (List (Symbol "begin" : exps))
    | null exps = return $ Bool False
    | otherwise = liftM last $ mapM (eval envRef) exps
eval envRef (List [Symbol "load", String modPath]) =
    evalLoadSF envRef modPath
eval envRef (List [Symbol "def-list"]) =
    evalDeflistSF envRef
eval envRef (List (sym : args)) = do
    fun <- eval envRef sym
    argVals <- mapM (eval envRef) args
    apply fun argVals
eval _ badForm = throwError $ BadFormError "unrecognized expression" badForm

createBuiltinEnv builtinFunctionsTable = do
    emptyEnv <- newIORef []
    let builtinCons (sym, fun) = (sym, BuiltinFunction fun)
    bindSymbols emptyEnv (map (builtinCons) builtinFunctionsTable)