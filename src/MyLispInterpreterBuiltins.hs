module MyLispInterpreterBuiltins (builtinFunctions) where

import Control.Monad.Error
import System.Exit
import System.IO

import MyLispInterpreterCore
import MyLispCommon
import MyLispUtils

{--
  ==========================================
  Numeric-domain, Numeric-codomain functions
  ==========================================
--}

ensureNumArgs :: [Expression] -> Execution ()
ensureNumArgs [] = return ()
ensureNumArgs (Integer _:args) = ensureNumArgs args
ensureNumArgs (Real _:args) = ensureNumArgs args
ensureNumArgs (arg:_) = throwError $ TypeError "number" arg

lookupNumBinFun "+" = (+)
lookupNumBinFun "-" = (-)
lookupNumBinFun "*" = (*)
lookupNumBinFun _ = error $ "INTERNAL ERROR: unexpected numeric bin. func.; " ++
    "this should never happen"

numBinFun :: String -> Expression -> Expression -> Expression
numBinFun funSym (Integer v1) (Integer v2) =
    Integer $ (lookupNumBinFun funSym) v1 v2
numBinFun funSym (Integer v1) (Real v2) =
    Real $ (lookupNumBinFun funSym) (fromIntegral v1) v2
numBinFun funSym (Real v1) (Integer v2) =
    Real $ (lookupNumBinFun funSym) v1 (fromIntegral v2)
numBinFun funSym (Real v1) (Real v2) = Real $ (lookupNumBinFun funSym) v1 v2
numBinFun _ _ _ = error $ "INTERNAL ERROR: unexpected numeric bin. func. " ++
    "operands; this should never happen"

applyNumBinFun :: String -> [Expression] -> Execution Expression
applyNumBinFun funSym args
    | null args = throwError $ NumArgsError funSym 1 []
    | otherwise = do
        ensureNumArgs args
        return $ foldl1 (numBinFun funSym) args

plusOp :: [Expression] -> Execution Expression
plusOp [] = return $ Integer 0
plusOp args = applyNumBinFun "+" args

minusOp :: [Expression] -> Execution Expression
minusOp [(Integer v)] = return $ Integer (-v)
minusOp [(Real v)] = return $ Real (-v)
minusOp args = applyNumBinFun "-" args

timesOp :: [Expression] -> Execution Expression
timesOp [] = return $ Integer 1
timesOp args = applyNumBinFun "*" args

{--
  ==========================================
  Integer-domain, Integer-codomain functions
  ==========================================
--}

lookupIntegerBinFun "quotient" = div
lookupIntegerBinFun "remainder" = rem
lookupIntegerBinFun "modulo" = mod

integerBinFun :: String -> [Expression] -> Execution Expression
integerBinFun funSym [(Integer v1), (Integer v2)] =
    return $ Integer $ (lookupIntegerBinFun funSym) v1 v2
integerBinFun _ [(Integer _), arg@_] = throwError $ TypeError "integer" arg
integerBinFun _ [arg@_, _] = throwError $ TypeError "integer" arg
integerBinFun funSym args = throwError $ NumArgsError funSym 2 args

{--
  =======================================
  Numeric-domain, Boolean-image functions
  =======================================
--}

lookupNumBoolBinFun "=" = (==)
lookupNumBoolBinFun "<" = (<)
lookupNumBoolBinFun ">" = (>)
lookupNumBoolBinFun "<=" = (<=)
lookupNumBoolBinFun ">=" = (>=)
lookupNumBoolBinFun _ = error $ "INTERNAL ERROR: unexpected (numeric -> " ++
    "bool) binary function; this should never happen"

numBoolBinFun :: String -> Expression -> Expression -> Bool
numBoolBinFun funSym (Integer v1) (Integer v2) =
    (lookupNumBoolBinFun funSym) v1 v2
numBoolBinFun funSym (Integer v1) (Real v2) =
    (lookupNumBoolBinFun funSym) (fromIntegral v1) v2
numBoolBinFun funSym (Real v1) (Integer v2) =
    (lookupNumBoolBinFun funSym) v1 (fromIntegral v2)
numBoolBinFun funSym (Real v1) (Real v2) = (lookupNumBoolBinFun funSym) v1 v2
numBoolBinFun _ _ _ = error $ "INTERNAL ERROR: unexpected (numeric -> bool) " ++
    "function operands; this should never happen"

applyNumBoolFun :: String -> [Expression] -> Execution Expression
applyNumBoolFun funSym args
    | length args < 2 = throwError $ NumArgsError funSym 2 args
    | otherwise = do
        ensureNumArgs args
        return $ Bool (foldrBool (numBoolBinFun funSym) args)

{--
  =================
  Number predicates
  =================
--}

isNumber :: [Expression] -> Execution Expression
isNumber args@[_] = isReal args
isNumber args = throwError $ NumArgsError "number?" 1 args

isReal :: [Expression] -> Execution Expression
isReal [(Real _)] = return $ Bool True
isReal args@[_] = isInteger args
isReal args = throwError $ NumArgsError "real?" 1 args

isInteger :: [Expression] -> Execution Expression
isInteger [(Integer _)] = return $ Bool True
isInteger [_] = return $ Bool False
isInteger args = throwError $ NumArgsError "integer?" 1 args

isExact :: [Expression] -> Execution Expression
isExact args@[_] = isInteger args
isExact args = throwError $ NumArgsError "exact?" 1 args

isInexact :: [Expression] -> Execution Expression
isInexact args@[_] = isReal args
isInexact args = throwError $ NumArgsError "inexact?" 1 args

{--
  ===========================
  Number conversion functions
  ===========================
--}

exactToInexact :: [Expression] -> Execution Expression
exactToInexact [(Integer v)] = return $ Real $ fromIntegral v
exactToInexact [exp@(Real _)] = return exp
exactToInexact [arg] = throwError $ TypeError "number" arg
exactToInexact args = throwError $ NumArgsError "exact->inexact" 1 args

inexactToExact :: [Expression] -> Execution Expression
inexactToExact [exp@(Integer _)] = return exp
inexactToExact [(Real v)] = return $ Integer $ round v
inexactToExact [arg] = throwError $ TypeError "number" arg
inexactToExact args = throwError $ NumArgsError "inexact->exact" 1 args

{--
  ==============
  List functions
  ==============
--}

pair :: [Expression] -> Execution Expression
pair [List xs] = return $ Bool $ (not . null) xs
pair [DottedList _ _] = return $ Bool True
pair [arg] = return $ Bool False
pair args = throwError $ NumArgsError "pair" 1 args

cons :: [Expression] -> Execution Expression
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List (x:xs)
cons [x, DottedList xs last] = return $ DottedList (x:xs) last
cons [x1, x2] = return $ DottedList [x1] x2
cons args = throwError $ NumArgsError "cons" 2 args

car :: [Expression] -> Execution Expression
car [List (h : _)] = return h
car [DottedList (h : _) _] = return h
car [arg] = throwError $ TypeError "non-empty list or dotted list" arg
car args = throwError $ NumArgsError "car" 1 args

cdr :: [Expression] -> Execution Expression
cdr [List (_ : t)] = return $ List t
cdr [DottedList [_] last] = return last
cdr [DottedList (_ : t) last] = return $ DottedList t last
cdr [arg] = throwError $ TypeError "non-empty list or dotted list" arg
cdr args = throwError $ NumArgsError "cdr" 1 args

{--
  ======================
  Equivalence predicates
  ======================
--}

equal :: [Expression] -> Execution Expression
equal [arg1, arg2] = liftE (checkEqual arg1 arg2) >>= return . Bool
    where
        checkEqual (Symbol v1) (Symbol v2) = return $ v1 == v2
        checkEqual (Bool v1) (Bool v2) = return $ v1 == v2
        checkEqual (Integer v1) (Integer v2) = return $ v1 == v2
        checkEqual (Real v1) (Real v2) = return $ v1 == v2
        checkEqual (Char v1) (Char v2) = return $ v1 == v2
        checkEqual (String v1) (String v2) = return $ v1 == v2
        checkEqual (List []) (List []) = return $ True
        checkEqual (List (x:xs)) (List (y:ys)) = do
            equalHead <- checkEqual x y
            equalTail <- checkEqual (List xs) (List ys)
            return $ equalHead && equalTail
        checkEqual (DottedList xs last1) (DottedList ys last2) =
            checkEqual (List $ xs ++ [last1]) (List $ ys ++ [last2])
        checkEqual arg1 arg2 = equalByPtr arg1 arg2
equal args = throwError $ NumArgsError "equal?" 2 args

apply' :: [Expression] -> Execution Expression
apply' [f, List args] = apply f args
apply' (f:args) = apply f args
apply' args = throwError $ NumArgsError "apply" 2 args

{--
  =================
  Utility functions
  =================
--}

defSourceCode :: [Expression] -> Execution Expression
defSourceCode [UserDefinedFunction params varArg body _] =
    return $ String $ "(lambda (" ++ unwords params ++ (
        case varArg of
            Nothing -> ""
            Just arg -> " . " ++ arg
        ) ++ ") " ++ unwords (map show body) ++ ")"
defSourceCode [BuiltinFunction _] = throwError $ GenericError "non-available"
defSourceCode [arg] = throwError $ TypeError "lambda object" arg
defSourceCode args = throwError $ NumArgsError "def-source-code" 1 args

exitSuccess' :: [Expression] -> Execution Expression
exitSuccess' [] = liftE $ exitSuccess >> return Void
exitSuccess' args = throwError $ NumArgsError "exit-success" 0 args

exitFailure' :: [Expression] -> Execution Expression
exitFailure' [] = liftE $ exitFailure >> return Void
exitFailure' args = throwError $ NumArgsError "exit-failure" 0 args

{--
  =======================
  Builtin functions table
  =======================
--}

builtinFunctions :: [(String, [Expression] -> Execution Expression)]
builtinFunctions = [("+", plusOp),
                    ("-", minusOp),
                    ("*", timesOp),
                    ("quotient", integerBinFun "quotient"),
                    ("remainder", integerBinFun "remainder"),
                    ("modulo", integerBinFun "modulo"),
                    ("=", applyNumBoolFun "="),
                    (">", applyNumBoolFun ">"),
                    ("<", applyNumBoolFun "<"),
                    (">=", applyNumBoolFun ">="),
                    ("<=", applyNumBoolFun "<="),
                    ("number?", isNumber),
                    ("real?", isReal),
                    ("integer?", isInteger),
                    ("exact?", isExact),
                    ("inexact?", isInexact),
                    ("exact->inexact", exactToInexact),
                    ("inexact->exact", inexactToExact),
                    ("pair?", pair),
                    ("cons", cons),
                    ("car", car),
                    ("cdr", cdr),
                    ("equal?", equal),
                    ("def-source-code", defSourceCode),
                    ("apply", apply'),
                    ("exit", exitSuccess'),
                    ("exit-failure", exitFailure')]