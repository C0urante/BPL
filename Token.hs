-- Chris Egerton
-- February 1-2, 2016

--      This is the Token module.
--      It contains definitions for the Token datatype as well as the enumerable
-- TokenType datatype.
--      It also specifies useful printing behavior for both, in order to
-- simplify debugging.

module Token where

data TokenType =
    T_IDENTIFIER |
    T_NUMBER |
    T_STRING_LITERAL |
    T_INT |
    T_VOID |
    T_STRING |
    T_IF |
    T_ELSE |
    T_WHILE |
    T_RETURN |
    T_WRITE |
    T_WRITELN |
    T_READ |
    T_SEMICOLON |
    T_COMMA |
    T_OPEN_BRACKET |
    T_CLOSE_BRACKET |
    T_OPEN_BRACE |
    T_CLOSE_BRACE |
    T_OPEN_PARENTHESIS |
    T_CLOSE_PARENTHESIS |
    T_LESS_THAN |
    T_LESS_THAN_OR_EQUAL |
    T_EQUAL |
    T_NOT_EQUAL |
    T_GREATER_THAN_OR_EQUAL |
    T_GREATER_THAN |
    T_PLUS |
    T_HYPHEN |
    T_ASTERISK |
    T_SLASH |
    T_PERCENT |
    T_AMPERSAND |
    T_ASSIGNMENT |
    T_END_OF_FILE
    deriving (Show, Enum, Eq)

tokenNumber :: Token -> String
tokenNumber = show . succ . fromEnum . token_type

data Token = Token {token_type  :: TokenType,
                    token_value :: String,
                    token_line  :: Int}
             deriving (Eq)

instance Show Token where
    show t = "Token " ++ (show $ token_type t) ++ " (" ++ (tokenNumber t) ++ "), " ++
             "string \"" ++ (token_value t) ++ "\", " ++
             "line number " ++ (show $ token_line t)
