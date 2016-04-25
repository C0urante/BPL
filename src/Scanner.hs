-- Chris Egerton
-- February 1-3, 2016

--      This is the Scanner module.
--      The only external function it provides is tokenize, which takes source
-- as a String and returns a list of Tokens.
--      Errors may be thrown if unterminated comments or strings are found,
-- or if invalid characters are encountered.
--      The list returned is infinite, containing all normal tokens found (in
-- order) followed by an infinite stream of the final EOF token.

module Scanner where

import Token
import Data.Maybe (fromJust)
import Data.Char (isDigit, isSpace, isAlpha, isAscii)
import qualified Data.Map as Map

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
     ("%", T_PERCENT),
     ("&", T_AMPERSAND),
     ("=", T_ASSIGNMENT)]

isAsciiAlpha :: Char -> Bool
isAsciiAlpha c = isAscii c && isAlpha c

isIdentifier :: Char -> Bool
isIdentifier c = c == '_' || isDigit c || isAsciiAlpha c

tokenize :: String -> [Token]
tokenize s = let (eof:ts) = tokenizer s [] 1 in
    reverse ts ++ repeat eof

tokenizer :: String -> [Token] -> Int -> [Token]
tokenizer "" acc line = Token T_END_OF_FILE "(EOF)" line:acc
tokenizer ('\n':source) acc line = tokenizer source acc (line + 1)
tokenizer ('"':source) acc line = parseString source acc line
tokenizer ('/':'*':source) acc line = skipBlockComment source acc line
tokenizer ('/':'/':source) acc line = skipLineComment source acc line
tokenizer (c1:c2:source) acc line
    | isSpace c1 =
        tokenizer (c2:source) acc line
    | Map.member [c1, c2] symbols =
        tokenizer source (symbolToken [c1, c2] line:acc) line
    | Map.member [c1] symbols =
        tokenizer (c2:source) (symbolToken [c1] line:acc) line
    | isDigit c1 =
        parseNumber (c1:c2:source) acc line
    | isAsciiAlpha c1 =
        parseWord (c1:c2:source) acc line
    | otherwise =
        badCharacter c1 line
tokenizer (c1:source) acc line
    | isSpace c1 =
        tokenizer source acc line
    | Map.member [c1] symbols =
        tokenizer source (symbolToken [c1] line:acc) line
    | isDigit c1 =
        parseNumber (c1:source) acc line
    | isAsciiAlpha c1 =
        parseWord (c1:source) acc line
    | otherwise =
        badCharacter c1 line

badCharacter :: Char -> Int -> [Token]
badCharacter c line =
    error $ "Unrecognized character at line " ++ show line ++": '" ++ (c:"'")

skipBlockComment :: String -> [Token] -> Int -> [Token]
skipBlockComment ('*':'/':source) acc line = tokenizer source acc line
skipBlockComment ('\n':source) acc line = skipBlockComment source acc (line + 1)
skipBlockComment (_:source) acc line = skipBlockComment source acc line
skipBlockComment "" _ _ =
    error $ "Unterminated comment encountered, " ++ "end of file reached."

skipLineComment :: String -> [Token] -> Int -> [Token]
skipLineComment ('\n':source) acc line = tokenizer source acc (line + 1)
skipLineComment (_:source) acc line = skipLineComment source acc line
skipLineComment "" acc line = tokenizer "" acc line

symbolToken :: String -> Int -> Token
symbolToken value = Token (fromJust $ Map.lookup value symbols) value

stringToken :: String -> Int -> Token
stringToken = Token T_STRING_LITERAL

numberToken :: String -> Int -> Token
numberToken = Token T_NUMBER

keywordToken :: String -> Int -> Token
keywordToken value = Token (fromJust $ Map.lookup value keywords) value

identifierToken :: String -> Int -> Token
identifierToken = Token T_IDENTIFIER

parseString :: String -> [Token] -> Int -> [Token]
parseString = helper "" where
    helper _ "" _ _ =
        error "Unterminated string encountered, end of file reached."
    helper current ('"':source) acc line =
        tokenizer source (stringToken (reverse current) line:acc) line
    helper current ('\\':'\\':source) acc line =
        helper ('\\':current) source acc line
    helper current ('\\':'"':source) acc line =
        helper ('"':current) source acc line
    helper _ ('\\':c:_) _ line =
        error $ "Unrecognized escape sequence: `\\" ++ [c] ++
                "' encountered at line " ++ show line
    helper current ('\n':source) acc line =
        helper ('\n':current) source acc (line + 1)
    helper current (c:source) acc line =
        helper (c:current) source acc line

parseNumber :: String -> [Token] -> Int -> [Token]
parseNumber source acc line = let (value, remainder) = span isDigit source in
    tokenizer remainder (numberToken value line:acc) line

parseWord :: String -> [Token] -> Int -> [Token]
parseWord source acc line = let (value, remainder) = span isIdentifier source in
    if Map.member value keywords
        then tokenizer remainder (keywordToken value line:acc) line
    else
        tokenizer remainder (identifierToken value line:acc) line
