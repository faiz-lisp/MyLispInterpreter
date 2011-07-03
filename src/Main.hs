module Main where

import System.IO

import MyLispCommon
import MyLispInterpreter

loop action = do
    p <- putStr ">>> " >> hFlush stdout >> getLine
    action p >>= putStrLn >> loop action

stdLib = do
    envRef <- startEnvironment
    interpreter envRef "(load \"lib/stdlib.scm\")" >>= putStrLn
    return envRef

main = stdLib >>= loop . (\envRef input -> interpreter envRef input)
