module ScannerTest where

import Scanner
import Token
import System.Environment (getArgs)

printTokens :: [Token] -> IO ()
printTokens [] =
    error "End of Token list reached while printing. This should not happen."
printTokens (t:ts)
    | tokenType t == T_END_OF_FILE = print t
    | otherwise = do
        print t
        printTokens ts

main :: IO ()
main = do
    args <- getArgs
    if null args
        then print "You must supply the name of a BPL file to tokenize."
    else
        do
            sourceCode <- readFile $ head args
            printTokens $ tokenize sourceCode
