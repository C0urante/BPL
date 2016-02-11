module Main where

import Scanner (tokenize)
import Parser (parseProgram)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    if null args
        then print "You must supply the name of a BPL file to tokenize."
    else
        do
            sourceCode <- readFile $ head args
            print $ parseProgram $ tokenize sourceCode
