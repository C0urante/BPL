module Main where

import Scanner (tokenize)
import Parser (parseProgram)
import Analyzer (processProgram)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    if null args
        then putStrLn "You must supply the name of a BPL file to type check."
    else
        do
            sourceCode <- readFile $ head args
            -- This is a hack. But, it does what I want.
            if length (show $ processProgram $ parseProgram $ tokenize sourceCode) > length "TypedProgram"
                then putStrLn "This program is type correct."
                else putStrLn "This program is type correct (although mysteriously short...)."
