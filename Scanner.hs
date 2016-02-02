import Token
import Data.Maybe (fromJust)
import Data.Char (isDigit, isSpace)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import qualified Data.Map as Map

-- int void string if else while return write writeln read
keywords :: Map.Map String TokenType
keywords = Map.fromList
    [("int", T_INT),
     ("void", T_VOID),
     ("string", T_STRING),
     ("if", T_IF),
     ("else", T_ELSE),
     ("while", T_WHILE),
     ("return", T_RETURN),
     ("write", T_WRITE),
     ("writeln", T_WRITELN),
     ("read", T_READ)]

-- ; , [ ] { } ( ) < <= == != >= > + - * / = % & /* */
symbols :: Map.Map String TokenType
symbols = Map.fromList
    [(";", T_SEMICOLON),
     (",", T_COMMA),
     ("[", T_OPEN_BRACKET),
     ("]", T_CLOSE_BRACKET),
     ("{", T_OPEN_BRACE),
     ("}", T_CLOSE_BRACE),
     ("(", T_OPEN_PARENTHESIS),
     (")", T_CLOSE_PARENTHESIS),
     ("<", T_LESS_THAN),
     ("<=", T_LESS_THAN_OR_EQUAL),
     ("==", T_EQUAL),
     ("!=", T_NOT_EQUAL),
     (">=", T_GREATER_THAN_OR_EQUAL),
     (">", T_GREATER_THAN),
     ("+", T_PLUS),
     ("-", T_HYPHEN),
     ("*", T_ASTERISK),
     ("/", T_SLASH),
     ("=", T_PERCENT),
     ("%", T_AMPERSAND),
     ("&", T_ASSIGNMENT)]

isAlpha :: Char -> Bool
isAlpha = (`elem` (['a'..'z'] ++ ['A'..'Z']))

isIdentifier :: Char -> Bool
isIdentifier c = c == '_' || isDigit c || isAlpha c

tokenize :: String -> [Token]
tokenize s = reverse $ tokenizer s [] 1

tokenizer :: String -> [Token] -> Int -> [Token]
tokenizer "" acc line = (Token T_END_OF_FILE "(EOF)" line):acc
tokenizer ('\n':source) acc line = tokenizer source acc (line + 1)
tokenizer ('"':source) acc line = parseString source acc line
tokenizer ('/':'*':source) acc line = skipComment source acc line
tokenizer (c1:c2:source) acc line =
    if isSpace c1 then
        tokenizer (c2:source) acc line
    else if Map.member [c1, c2] symbols
        then tokenizer source ((symbolToken [c1, c2] line):acc) line
    else if Map.member [c1] symbols
        then tokenizer (c2:source) ((symbolToken [c1] line):acc) line
    else if isDigit c1
        then parseNumber (c1:c2:source) acc line
    else if isAlpha c1
        then parseWord (c1:c2:source) acc line
    else
        badCharacter c1 line
tokenizer (c1:source) acc line =
    if isSpace c1 then
        tokenizer source acc line
    else if Map.member [c1] symbols
        then tokenizer source ((symbolToken [c1] line):acc) line
    else if isDigit c1
        then parseNumber (c1:source) acc line
    else if isAlpha c1
        then parseWord (c1:source) acc line
    else
        badCharacter c1 line

badCharacter :: Char -> Int -> [Token]
badCharacter c line = error $ "Unrecognized character at line " ++ (show line) ++ ": '" ++ (c:"'")

skipComment :: String -> [Token] -> Int -> [Token]
skipComment ('*':'/':source) acc line = tokenizer source acc line
skipComment ('\n':source) acc line = skipComment source acc (line + 1)
skipComment (_:source) acc line = skipComment source acc line
skipComment "" _ _ = error "Unterminated comment encountered, end of file reached."

symbolToken :: String -> Int -> Token
symbolToken value line = Token (fromJust $ Map.lookup value symbols) value line

numberToken :: String -> Int -> Token
numberToken = Token T_NUMBER

keywordToken :: String -> Int -> Token
keywordToken value line = Token (fromJust $ Map.lookup value keywords) value line

identifierToken :: String -> Int -> Token
identifierToken = Token T_IDENTIFIER

parseString :: String -> [Token] -> Int -> [Token]
parseString source acc line = helper "" source acc line where
    helper _ "" _ _ =
        error "Unterminated string encountered, end of file reached."
    helper current ('"':source) acc line =
        tokenizer source ((Token T_STRING_LITERAL (reverse current) line):acc) line
    helper current ('\\':'\\':source) acc line =
        helper ('\\':current) source acc line
    helper current ('\\':'"':source) acc line =
        helper ('"':current) source acc line
    helper current (c:source) acc line =
        helper (c:current) source acc line

parseNumber :: String -> [Token] -> Int -> [Token]
parseNumber source acc line =
    let (value, remainder) = span isDigit source
        isValidDelimiter r = isSpace (head r) || Map.member (take 1 r) symbols || Map.member (take 2 r) symbols
    in
        if isValidDelimiter remainder
            then tokenizer remainder ((numberToken value line):acc) line
        else
            error $ "Invalid delimiter encountered after number token: '" ++ ((head remainder):"'")

parseWord :: String -> [Token] -> Int -> [Token]
parseWord source acc line = let (value, remainder) = span isIdentifier source in
    if Map.member value keywords
        then tokenizer remainder ((keywordToken value line):acc) line
    else
        tokenizer remainder ((identifierToken value line):acc) line

printTokens :: [Token] -> IO ()
printTokens [] = print "No tokens found."
printTokens (t:[]) = print t
printTokens (t:ts) = do
    print t
    printTokens ts

-- You should turn in a program that asks the
-- user for the name of a BPL file, opens this file, and repeatedly calls getNextToken(), printing each
-- token found until it gets to the end of the file.
main :: IO ()
main = do
    args <- getArgs
    if null args
        then print "You must supply the name of a BPL file to tokenize."
    else
        do
            sourceCode <- readFile $ head args
            printTokens $ tokenize sourceCode
