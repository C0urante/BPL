module Main where

import System.Environment (getArgs)
import System.IO

import Scanner (tokenize)
import Parser (parseProgram)
import Analyzer (processProgram)
import Optimizer (optimize)
import Compiler (compile)

sourceToAssembly :: String -> String
sourceToAssembly = unlines . map show . compile . optimize . processProgram . parseProgram . tokenize

assemblyFilename :: String -> String
assemblyFilename s = if extension == ".bpl"
    then basename ++ ".s"
    else error $ "Invalid file extension: '" ++ extension ++ "'; must end with '.bpl'" where
        extension = reverse $ take 4 $ reverse s
        basename = reverse $ takeWhile (/= '/') $ drop 4 $ reverse s

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1
        then putStrLn "You must supply the name of exactly one BPL file to compile."
    else
        let
            sourceFilename = head args
            outputName = assemblyFilename sourceFilename in
            do
                outputFile <- openFile outputName WriteMode
                sourceCode <- readFile sourceFilename
                hPutStr outputFile $ sourceToAssembly sourceCode
                hClose outputFile
