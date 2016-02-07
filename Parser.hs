module Parser where

import Token
import Grammar
import Data.Maybe

-- IDEA: General structure of parsing functions:
--   Only ever take in Token list and (except for top level function) callback
-- for parameters.
--   Only ever calculate necessary return value, then give return value and
-- rest of Token list back to callback.
--   Contain own helper callback functions in everything except terminal
-- functions, which then use tail recursion and partial application to pass one
-- of themselves as a callback to the necessary subordinate functions while
-- keeping track of the growing elements of the return value.

-- 1. PROGRAM -> DECLARATION_LIST
-- 2. DECLARATION_LIST -> DECLARATION_LIST DECLARATION | DECLARATION
-- 3. DECLARATION -> VAR_DEC | FUN_DEC
-- 4. VAR_DEC -> TYPE_SPECIFIER <id> ;
--             | TYPE_SPECIFIER *<id> ;
--             | TYPE_SPECIFIER <id>[ <num> ] ;
-- 5. TYPE_SPECIFIER -> int | void | string
-- 6. FUN_DEC -> TYPE_SPECIFIER <id> ( PARAMS ) COMPOUND_STMT
-- 7. PARAMS -> void | PARAM_LIST
-- 8. PARAM_LIST -> PARAM_LIST , PARAM | PARAM
-- 9. PARAM -> TYPE_SPECIFIER <id>
--           | TYPE_SPECIFIER *<id>
--           | TYPE_SPECIFIER <id>[ ]
-- 10. COMPOUND_STMT -> { LOCAL_DECS STATEMENT_LIST }
-- 11. LOCAL_DECS -> LOCAL_DECS VAR_DEC | <empty>
-- 12. STATEMENT_LIST -> STATEMENT_LIST STATEMENT | <empty>
-- 13. STATEMENT -> EXPRESSION_STMT
--                | COMPOUND_STMT
--                | IF_STMT
--                | WHILE_STMT
--                | RETURN_STMT
--                | WRITE_STMT
-- 14. EXPRESSION_STMT -> EXPRESSION ; | ;
-- 15. IF_STMT -> if ( EXPRESSION ) STATEMENT
--              | if ( EXPRESSION ) STATEMENT else STATEMENT
-- 16. WHILE_STMT -> while ( EXPRESSION ) statement
-- 17. RETURN_STMT -> return ; | return EXPRESSION ;
-- 18. WRITE_STMT -> write ( EXRESSION ) ; | writeln ( ) ;
-- 19. EXPRESSION -> VAR = EXPRESSION | COMP_EXP
-- 20. VAR -> <id> | <id>[ EXPRESSION ] | *<id>
-- 21. COMP_EXP -> E RELOP E | E
-- 22. RELOP -> <= | < | == | != | > | >=
-- 23. E -> E ADDOP T | T
-- 24. ADDOP -> + | -
-- 25. T -> T MULOP F | F
-- 26. MULOP -> * | / | %
-- 27. F -> -F | &Factor | *Factor | Factor
-- 28. Factor -> ( EXPRESSION ) |
                --  FUN_CALL |
                --  read ( ) |
                --  *<id> |
                --  <id> |
                --  <id>[EXPRESSION] |
                --  <num> |
                --  <string>
-- 29. FUN_CALL -> <id> ( ARGS )
-- 30. ARGS -> ARG_LIST | <empty>
-- 31. ARG_LIST -> ARG_LIST , EXPRESSION | EXPRESSION

-- 1. PROGRAM -> DECLARATION_LIST
parseProgram :: [Token] -> Program
parseProgram = parseDeclarationList helper where
    helper decs _ = Program decs

-- 2. DECLARATION_LIST -> DECLARATION_LIST DECLARATION | DECLARATION
parseDeclarationList :: (DeclarationList -> [Token] -> a) -> [Token] -> a
parseDeclarationList cb = parseDeclaration (helper []) where
    -- helper :: [Declaration] -> Declaration -> [Token] -> a
    helper acc dec [Token T_END_OF_FILE _ _] =
        cb (DeclarationList $ reverse (dec:acc)) []
    helper acc dec ts = parseDeclaration (helper (dec:acc)) ts

-- 3. DECLARATION -> VAR_DEC | FUN_DEC
parseDeclaration :: (Declaration -> [Token] -> a) -> [Token] -> a
parseDeclaration cb = parseVarDec varDecHelper where
    -- varDecHelper :: Maybe VarDec -> [Token] -> a
    varDecHelper Nothing ts = parseFunDec funDecHelper ts
    varDecHelper (Just vdec) ts = cb (VarDecDeclaration vdec) ts
    -- funDecHelper :: Maybe FunDec -> [Token] -> a
    funDecHelper Nothing _ = error "Expected Declaration"
    funDecHelper (Just fdec) ts = cb (FunDecDeclaration fdec) ts

-- 4. VAR_DEC -> TYPE_SPECIFIER <id> ;
--             | TYPE_SPECIFIER *<id> ;
--             | TYPE_SPECIFIER <id>[ <num> ] ;
parseVarDec :: (Maybe VarDec -> [Token] -> a) -> [Token] -> a
parseVarDec cb [] = unexpectedEnd
parseVarDec cb [Token T_END_OF_FILE v l] = cb Nothing [Token T_END_OF_FILE v l]
parseVarDec cb tokens = parseTypeSpecifier helper tokens where
    -- helper :: (Maybe TypeSpecifier) -> [Token] -> a
    helper Nothing ts = cb Nothing ts
    helper (Just t) (Token T_IDENTIFIER i _:
                     Token T_SEMICOLON _ _:ts) =
        cb (Just $ VarDec t RawVarDec i) ts
    helper (Just t) (Token T_ASTERISK _ _:
                     Token T_IDENTIFIER i _:
                     Token T_SEMICOLON _ _:ts) =
        cb (Just $ VarDec t PointerVarDec i) ts
    helper (Just t) (Token T_IDENTIFIER i _:
                     Token T_OPEN_BRACKET _ _:
                     Token T_NUMBER n _:
                     Token T_CLOSE_BRACKET _ _:
                     Token T_SEMICOLON _ _:ts) =
        cb (Just $ VarDec t (ArrayVarDec $ parseInt n) i) ts

-- 5. TYPE_SPECIFIER -> int | void | string
parseTypeSpecifier :: (Maybe TypeSpecifier -> [Token] -> a) -> [Token] -> a
parseTypeSpecifier cb [] = unexpectedEnd
parseTypeSpecifier cb (Token T_INT _ _:ts) = cb (Just IntType) ts
parseTypeSpecifier cb (Token T_STRING _ _:ts) = cb (Just StringType) ts
parseTypeSpecifier cb (Token T_VOID _ _:ts) = cb (Just VoidType) ts
parseTypeSpecifier cb ts = cb Nothing ts

-- 6. FUN_DEC -> TYPE_SPECIFIER <id> ( PARAMS ) COMPOUND_STMT
parseFunDec :: (Maybe FunDec -> [Token] -> a) -> [Token] -> a
parseFunDec cb [] = unexpectedEnd
parseFunDec cb tokens = parseTypeSpecifier typeSpecifierHelper tokens where
    -- typeSpecifierHelper :: (Maybe TypeSpecifier -> [Tokens] -> a) -> [Tokens] a
    typeSpecifierHelper Nothing ts = cb Nothing ts
    typeSpecifierHelper (Just t) (Token T_IDENTIFIER i l1:
                                  Token T_OPEN_PARENTHESIS _ l2:ts) =
        parseParams (paramsHelper (Token T_IDENTIFIER i l1:Token T_OPEN_PARENTHESIS "(" l2:ts)) ts
    typeSpecifierHelper _ ts = cb Nothing ts
    -- paramsHelper :: [Token] -> (Maybe Params -> [Token] -> a) -> [Token] -> a
    paramsHelper ps Nothing ts = cb Nothing (ps:ts)
    paramsHelper (Just p) (Token T_CLOSE_PARENTHESIS _ _:ts) =
        parseCompoundStmt compoundStmtHelper ts
    paramsHelper _ ts = cb Nothing ts
    -- compoundStmtHelper :: (Maybe CompoundStmt -> [Token] -> a) -> [Token] -> a
    compoundStmtHelper Nothing ts = cb Nothing ts

-- 7. PARAMS -> void | PARAM_LIST
-- 8. PARAM_LIST -> PARAM_LIST , PARAM | PARAM
-- 9. PARAM -> TYPE_SPECIFIER <id>
--           | TYPE_SPECIFIER *<id>
--           | TYPE_SPECIFIER <id>[ ]
-- 10. COMPOUND_STMT -> { LOCAL_DECS STATEMENT_LIST }
-- 11. LOCAL_DECS -> LOCAL_DECS VAR_DEC | <empty>
-- 12. STATEMENT_LIST -> STATEMENT_LIST STATEMENT | <empty>
-- 13. STATEMENT -> EXPRESSION_STMT
--                | COMPOUND_STMT
--                | IF_STMT
--                | WHILE_STMT
--                | RETURN_STMT
--                | WRITE_STMT
-- 14. EXPRESSION_STMT -> EXPRESSION ; | ;
-- 15. IF_STMT -> if ( EXPRESSION ) STATEMENT
--              | if ( EXPRESSION ) STATEMENT else STATEMENT
-- 16. WHILE_STMT -> while ( EXPRESSION ) statement
-- 17. RETURN_STMT -> return ; | return EXPRESSION ;
-- 18. WRITE_STMT -> write ( EXRESSION ) ; | writeln ( ) ;
-- 19. EXPRESSION -> VAR = EXPRESSION | COMP_EXP
-- 20. VAR -> <id> | <id>[ EXPRESSION ] | *<id>
-- 21. COMP_EXP -> E RELOP E | E
-- 22. RELOP -> <= | < | == | != | > | >=
-- 23. E -> E ADDOP T | T
-- 24. ADDOP -> + | -
-- 25. T -> T MULOP F | F
-- 26. MULOP -> * | / | %
-- 27. F -> -F | &Factor | *Factor | Factor
-- 28. Factor -> ( EXPRESSION ) |
                --  FUN_CALL |
                --  read ( ) |
                --  *<id> |
                --  <id> |
                --  <id>[EXPRESSION] |
                --  <num> |
                --  <string>
-- 29. FUN_CALL -> <id> ( ARGS )
-- 30. ARGS -> ARG_LIST | <empty>
-- 31. ARG_LIST -> ARG_LIST , EXPRESSION | EXPRESSION

parseInt :: String -> Int
parseInt = read

unexpectedToken :: TokenType -> TokenType -> Int -> a
unexpectedToken t1 t2 line =
    error $ "Line " ++ show line ++
            ": expected token type " ++ show t1 ++
            ", found type " ++ show t2 ++ " instead"

unexpectedEnd :: a
unexpectedEnd =
    error "Encountered unexpected end of token stream (this should never happen)"
