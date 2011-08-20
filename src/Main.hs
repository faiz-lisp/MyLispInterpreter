module Main where

import Control.Exception
import Control.Monad
import Data.Version
import Prelude hiding (catch)
import System
import System.Console.GetOpt
import System.Info
import System.IO

import MyLispInterpreter

data Options = Options {
    optVerbose :: Bool,
    optShowHelp :: Bool,
    optRepl :: Bool,
    optStdLib :: String
} deriving Show

defaultOptions = Options {
    optVerbose = False,
    optShowHelp = False,
    optRepl = False,
    optStdLib = "stdlib.scm"
}

optionsDescr :: [OptDescr (Options -> Options)]
optionsDescr = [
    Option
        "h"
        ["help"]
        (NoArg (\opts -> opts { optShowHelp = True }))
        "show this help",
    Option
        "v"
        ["verbose"]
        (NoArg (\opts -> opts { optVerbose = True }))
        "verbose mode",
    Option
        "i"
        ["repl"]
        (NoArg (\opts -> opts { optRepl = True }))
        "read-eval-print loop",
    Option
        "l"
        ["stdlib"]
        (ReqArg (\arg opts -> opts { optStdLib = arg }) "FILE")
        "standard library path"
    ]

versionNumber = "0.0.1beta"

versionHeader = "MyLispInterpreter v" ++ versionNumber ++ " - " ++
    compilerName ++ (showVersion compilerVersion)

usageMsg = do
    progName <- getProgName
    let header = versionHeader ++ "\n\nUsage: " ++ progName ++
                    " [OPTION ...] [FILE ...]\n"
    return $ usageInfo header optionsDescr

processCmdLine :: IO (Options, [String])
processCmdLine = do
    args <- getArgs
    case getOpt RequireOrder optionsDescr args of
        (options, nonOptions, []) -> return $
            (foldl (flip id) defaultOptions options, nonOptions)
        (_, _, errors) ->
            usageMsg >>= (\m -> return $ (concat errors) ++ '\n':m) >>=
            hPutStr stderr >> exitWith (ExitFailure 1)

replWelcomeMsg = versionHeader ++
    "\n\nA really tiny Scheme interpreter." ++
    "\n\nType (def-list) to inspect the environment."

repl evaluator = putStrLn replWelcomeMsg >> forever (do
    putStr ">>> " >> hFlush stdout
    out <- getLine >>= evaluator
    if null out
        then return ()
        else putStrLn out)

main = do
    (options, files) <- processCmdLine
    let Options {
        optVerbose = optVerbose,
        optShowHelp = optShowHelp,
        optRepl = optRepl,
        optStdLib = optStdLib
    } = options
    when optShowHelp (usageMsg >>= putStr >> exitWith ExitSuccess)
    interpreter <- getMyLispInterpreter "stdin"
    let startupCmds = concat ["(load \"" ++ m ++ "\")" | m <- optStdLib:files]
    let onAbort e = putStrLn $ "\nAborted: " ++ (show (e :: SomeException))
    handle onAbort (do
        interpreter startupCmds >>= (\out -> if not optVerbose || null out
                                                then return ()
                                                else putStrLn out)
        when (null files || optRepl) (repl interpreter))