module MyLispCommon where

import Control.Monad.Error
import Data.IORef
import Text.ParserCombinators.Parsec

import MyLispUtils

data Expression = Symbol String
                | Bool Bool
                | Integer Integer
                | String String
                | List [Expression]
                | DottedList [Expression] Expression
                | BuiltinFunction ([Expression] -> Execution Expression)
                | BuiltinIOFunction ([Expression] -> ExecutionIO Expression)
                | UserDefinedFunction [String] (Maybe String) [Expression]
                      Environment

instance Show Expression where
    show (Symbol i) = i
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (Integer i) = show i
    show (String s) = "\"" ++ s ++ "\""
    show (List l) = "(" ++ unwordsShow l ++ ")"
    show (DottedList h t) = "(" ++  unwordsShow h ++  " . " ++ show t ++ ")"
    show (BuiltinFunction _) = "<primitive-function>"
    show (BuiltinIOFunction _) = "<primitive-IO-function>"
    show (UserDefinedFunction params varArg body _) =
        "(lambda (" ++ unwords params ++ (case varArg of
            Nothing -> ""
            Just arg -> " . " ++ arg) ++ ") ...)"

data ExecutionError = ParserError ParseError
                    | NumArgsError Integer [Expression]
                    | TypeError String Expression
                    | UnboundSymbolError String String
                    | BadFormError String Expression
                    | NotFunctionError String String
                    | DefaultError String

instance Show ExecutionError where
    show (ParserError e) = "parser error at " ++ show e
    show (NumArgsError e f) = "incorrect number of arguments in: " ++
        unwordsShow f ++ "; expected " ++ show e
    show (TypeError e f) = "invalid type: expected " ++ e ++ "; given " ++
        show f
    show (UnboundSymbolError m v) = m ++ ": " ++ v
    show (BadFormError m f) = m ++ ": " ++ show f
    show (NotFunctionError m f) = m ++ ": " ++ show f
    show (DefaultError s) = "error: " ++ s

instance Error ExecutionError where
    noMsg = DefaultError "an unknown error has ocurred"
    strMsg = DefaultError

catchErrorMsg :: (MonadError e m, Show e) => m String -> m String
catchErrorMsg action = catchError action (return . show)

type Execution a = Either ExecutionError a

unpackExecution :: Execution a -> a
unpackExecution (Right e) = e

type ExecutionIO a = ErrorT ExecutionError IO a

liftExecution :: Execution a -> ExecutionIO a
liftExecution (Left e) = throwError e
liftExecution (Right v) = return v

runExecutionIO :: ExecutionIO String -> IO String
runExecutionIO action = runErrorT (catchErrorMsg action) >>=
    return . unpackExecution

type Environment = IORef [(String, IORef Expression)]
