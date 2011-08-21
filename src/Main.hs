{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception
import Control.Monad
import Data.Version
import Prelude hiding (catch)
import System
import System.Console.GetOpt
import System.Exit
import System.Info
import System.IO

import MyLispInterpreter

data Options = Options {
    optVerbose :: Bool,
    optShowHelp :: Bool,
    optRepl :: Bool,
    optStdLib :: String
}

defaultOptions = Options {
    optVerbose = False,
    optShowHelp = False,
    optRepl = False,
    optStdLib = ""
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
    case getOpt Permute optionsDescr args of
        (options, nonOptions, []) -> return
            (foldl (flip id) defaultOptions options, nonOptions)
        (_, _, errors) ->
            usageMsg >>= (\m -> return $ (concat errors) ++ '\n':m) >>=
            hPutStr stderr >> exitFailure

repl evaluator = putStrLn welcomeMsg >> forever (do
    putStr ">>> " >> hFlush stdout
    out <- isEOF >>= (\eof -> if eof then return "(exit)"
                                     else getLine) >>= evaluator
    if null out
        then return ()
        else putStrLn out)
    where
        welcomeMsg = versionHeader ++
            "\n\nA really tiny Scheme interpreter." ++
            "\n\nType (def-list) to inspect the environment."

run = do
    (options, files) <- processCmdLine

    let Options {
        optVerbose = optVerbose,
        optShowHelp = optShowHelp,
        optRepl = optRepl,
        optStdLib = optStdLib
    } = options

    when optShowHelp $ usageMsg >>= putStr >> exitSuccess

    interpreter <- getMyLispInterpreter "stdin"

    let onAbort e = putStrLn ("\nAborted: " ++ show (e :: AsyncException)) >>
                        exitFailure
    let startupCmds = concat ["(load \"" ++ m ++ "\")" |
                        m <- optStdLib:files, (not . null) m]
    let batchInterpreter = handle (\e -> putStrLn (show (e :: ErrorCall)) >>
                                        exitFailure)
                                    (interpreter startupCmds) >>=
                            (\out -> if not optVerbose || null out
                                        then return ()
                                        else putStrLn out)
    let replInterpreter = repl $ (\input -> handle
                                    (\e -> return $ show (e :: ErrorCall))
                                    (interpreter input))

    handle onAbort $
        batchInterpreter >> when (null files || optRepl) replInterpreter

main =
    run
    `catches`
    [Handler (\(e :: ExitCode) -> throw e),
     Handler (\(e :: SomeException) ->
        putStrLn ("=O THE UNTHINKABLE HAPPENED =O!! Fill a bug report. " ++
            "Error details: " ++ show (e :: SomeException)) >>
        exitFailure)]