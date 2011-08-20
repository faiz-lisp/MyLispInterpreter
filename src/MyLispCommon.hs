module MyLispCommon where

import Control.Monad.Error
import Data.IORef

import MyLispUtils

data Expression = Void
                | Symbol String
                | Bool Bool
                | Integer Integer
                | Real Double
                | Char Char
                | String String
                | List [Expression]
                | DottedList [Expression] Expression
                | BuiltinFunction ([Expression] -> Execution Expression)
                | UserDefinedFunction [String] (Maybe String) [Expression]
                      Environment

instance Show Expression where
    show (Void) = ""
    show (Symbol s) = s
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (Integer i) = show i
    show (Real r) = show r
    show (String s) = show s
    show (Char c) = '#':'\\':c:""
    show (List l) = "(" ++ unwordsShow l ++ ")"
    show (DottedList h t) = "(" ++  unwordsShow h ++  " . " ++ show t ++ ")"
    show (BuiltinFunction _) = "<builtin-function>"
    show (UserDefinedFunction params varArg body _) =
        "(lambda (" ++ unwords params ++ (
            case varArg of
                Nothing -> ""
                Just arg -> " . " ++ arg
            ) ++ ") ...)"

data ExecutionError = ParserError String
                    | NumArgsError String Int [Expression]
                    | TypeError String Expression
                    | UnboundSymbolError String String
                    | BadFormError String Expression
                    | IOError String
                    | GenericError String

instance Show ExecutionError where
    show (ParserError e) = "#{parser error} " ++ e
    show (NumArgsError f nArgs e) = "incorrect number of arguments in: " ++
        f ++ "; expected " ++ show nArgs ++ "; given " ++ unwordsShow e
    show (TypeError et ge) = "#{type error} expected " ++ et ++ "; given " ++
        show ge
    show (UnboundSymbolError m v) = m ++ ": " ++ v
    show (BadFormError m f) = "#{bad form error} " ++ m ++ ": " ++ show f
    show (IOError m) = "#{IO error} " ++ m
    show (GenericError m) = "#{error} " ++ m

instance Error ExecutionError where
    noMsg = GenericError "an unknown error has ocurred"
    strMsg = GenericError

newtype Execution a = E {
    runE :: ErrorT ExecutionError IO a
} deriving (Monad, MonadError ExecutionError)

liftE :: IO a -> Execution a
liftE e = E $ lift e

type Environment = IORef [(String, IORef Expression)]