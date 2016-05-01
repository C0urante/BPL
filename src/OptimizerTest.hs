module Main where

import Scanner (tokenize)
import Parser (parseProgram)
import Analyzer (processProgram)
import Optimizer (optimize)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    if null args
        then putStrLn "You must supply the name of a BPL file to type check."
    else
        do
            sourceCode <- readFile $ head args
            print $ optimize $ processProgram $ parseProgram $ tokenize sourceCode
