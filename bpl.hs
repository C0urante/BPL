module Main where

import Data.List (intercalate)
import System.Environment (getArgs)

import Scanner (tokenize)
import Parser (parseProgram)
import Analyzer (processProgram)
import Compiler (compile)

main :: IO ()
main = do
    args <- getArgs
    if null args
        then putStrLn "You must supply the name of a BPL file to tokenize."
    else
        do
            sourceCode <- readFile $ head args
            putStrLn $ intercalate "\n" $ map show $ compile $ processProgram $ parseProgram $ tokenize sourceCode
