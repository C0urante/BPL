module Main where

import Scanner (tokenize)
import Parser (parseProgram)
import Analyzer (analyze)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    if null args
        then print "You must supply the name of a BPL file to tokenize."
    else
        do
            sourceCode <- readFile $ head args
            putStrLn $ seq (analyze $ parseProgram $ tokenize sourceCode) "This program's types are correct."
