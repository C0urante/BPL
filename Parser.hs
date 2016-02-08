module Parser where

import Token
import Grammar

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
    -- helper :: DeclarationList -> [Token] -> Program
    helper decList _ = Program decList

-- 2. DECLARATION_LIST -> DECLARATION_LIST DECLARATION | DECLARATION
parseDeclarationList :: (DeclarationList -> [Token] -> a) -> [Token] -> a
parseDeclarationList cb = parseDeclaration (helper []) where
    -- helper :: [Declaration] -> Declaration -> [Token] -> a
    helper acc dec [Token T_END_OF_FILE v l] =
        cb (DeclarationList $ reverse (dec:acc)) [Token T_END_OF_FILE v l]
    helper acc dec ts =
        parseDeclaration (helper (dec:acc)) ts

-- 3. DECLARATION -> VAR_DEC | FUN_DEC
-- 4. VAR_DEC -> TYPE_SPECIFIER <id> ;
--             | TYPE_SPECIFIER *<id> ;
--             | TYPE_SPECIFIER <id>[ <num> ] ;
-- 6. FUN_DEC -> TYPE_SPECIFIER <id> ( PARAMS ) COMPOUND_STMT
-- First consume type specification, identifier, and possibly metatype.
-- Then, if parsing function declaration, consume parentheses, params, and
--  compound statement, returning the resulting FunDec.
parseDeclaration :: (Declaration -> [Token] -> a) -> [Token] -> a
parseDeclaration cb = parseTypeSpecifier typeSpecifierHelper where
    -- typeSpecifierHelper :: TypeSpecifier -> [Token] -> a
    typeSpecifierHelper t (Token T_IDENTIFIER i _:
                           Token T_SEMICOLON _ _:ts) =
        cb (VarDecDeclaration $ VarDec t RawVarDec i) ts
    typeSpecifierHelper t (Token T_ASTERISK _ _:
                           Token T_IDENTIFIER i _:
                           Token T_SEMICOLON _ _:ts) =
        cb (VarDecDeclaration $ VarDec t PointerVarDec i) ts
    typeSpecifierHelper t (Token T_IDENTIFIER i _:
                           Token T_OPEN_BRACKET _ _:
                           Token T_NUMBER n _:
                           Token T_CLOSE_BRACKET _ _:
                           Token T_SEMICOLON _ _:ts) =
        cb (VarDecDeclaration $ VarDec t (ArrayVarDec $ parseInt n) i) ts
    typeSpecifierHelper t (Token T_IDENTIFIER i _:
                           Token T_OPEN_PARENTHESIS _ _:ts) =
        parseParams (paramsHelper t i) ts
    typeSpecifierHelper _ (Token _ v l:_) =
        error $ "Line " ++ show l ++ ": " ++
                "expected variable declaration or function declaration; " ++
                "found " ++ show v ++ " instead"
    -- paramsHelper :: TypeSpecifier -> Identifier -> Params -> [Token] -> a
    paramsHelper t i p (Token T_CLOSE_PARENTHESIS _ _:ts) =
        parseCompoundStmt (compoundStmtHelper t i p) ts
    paramsHelper _ _ _ (Token _ v l:_) =
        error $ "Line " ++ show l ++
                ": expected \")\"; " ++
                "found " ++ show v ++ " instead"
    -- compoundStmtHelper :: TypeSpecifier Identifier Param CompoundStmt -> a
    compoundStmtHelper t i p c =
        cb (FunDecDeclaration $ FunDec t i p c)

-- 4. VAR_DEC -> TYPE_SPECIFIER <id> ;
--             | TYPE_SPECIFIER *<id> ;
--             | TYPE_SPECIFIER <id>[ <num> ] ;
parseVarDec :: (VarDec -> [Token] -> a) -> [Token] -> a
parseVarDec cb = parseTypeSpecifier helper where
    -- helper :: TypeSpecifier -> [Token] -> a
    helper t (Token T_IDENTIFIER i _:
              Token T_SEMICOLON _ _:ts) =
        cb (VarDec t RawVarDec i) ts
    helper t (Token T_ASTERISK _ _:
              Token T_IDENTIFIER i _:
              Token T_SEMICOLON _ _:ts) =
        cb (VarDec t PointerVarDec i) ts
    helper t (Token T_IDENTIFIER i _:
              Token T_OPEN_BRACKET _ _:
              Token T_NUMBER n _:
              Token T_CLOSE_BRACKET _ _:
              Token T_SEMICOLON _ _:ts) =
        cb (VarDec t (ArrayVarDec $ parseInt n) i) ts
    helper _ (Token _ v l:_) =
        error $ "Line " ++ show l ++ ": " ++
                "expected variable declaration; " ++
                "found " ++ show v ++ " instead"

-- 5. TYPE_SPECIFIER -> int | void | string
parseTypeSpecifier :: (TypeSpecifier -> [Token] -> a) -> [Token] -> a
parseTypeSpecifier cb (Token T_INT _ _:ts) = cb IntType ts
parseTypeSpecifier cb (Token T_STRING _ _:ts) = cb StringType ts
parseTypeSpecifier cb (Token T_VOID _ _:ts) = cb VoidType ts
parseTypeSpecifier _ (Token _ v l:_) =
    error $ "Line " ++ show l ++ ": " ++
            "expected int, string, or void; " ++
            "found " ++ show v ++ " instead"

-- 7. PARAMS -> void | PARAM_LIST
parseParams :: (Params -> [Token] -> a) -> [Token] -> a
parseParams cb (Token T_VOID _ _:ts) = cb EmptyParams ts
parseParams cb ts = parseParamList helper ts where
    -- helper :: ParamList -> [Token] -> a
    helper = cb . Params

-- 8. PARAM_LIST -> PARAM_LIST , PARAM | PARAM
parseParamList :: (ParamList -> [Token] -> a) -> [Token] -> a
parseParamList cb = parseParam (helper []) where
    -- helper :: [Param] -> Param -> [Token] -> a
    helper acc p (Token T_COMMA _ _:ts) = parseParam (helper (p:acc)) ts
    helper acc p ts = cb (ParamList $ reverse (p:acc)) ts

-- 9. PARAM -> TYPE_SPECIFIER <id>
--           | TYPE_SPECIFIER *<id>
--           | TYPE_SPECIFIER <id>[ ]
parseParam :: (Param -> [Token] -> a) -> [Token] -> a
parseParam cb = parseTypeSpecifier typeSpecifierHelper where
    -- typeSpecifierHelper :: TypeSpecifier -> [Token] -> a
    typeSpecifierHelper t (Token T_IDENTIFIER i _:
                     Token T_OPEN_BRACKET _ _:
                     Token T_CLOSE_BRACKET _ _:ts) =
        cb (Param t ArrayParam i) ts
    typeSpecifierHelper t (Token T_IDENTIFIER i _:ts) =
        cb (Param t RawParam i) ts
    typeSpecifierHelper t (Token T_ASTERISK _ _:
                           Token T_IDENTIFIER i _:ts) =
        cb (Param t PointerParam i) ts
    typeSpecifierHelper _ (Token _ v l:_) =
        error $ "Line " ++ show l ++ ": " ++
                "expected param; " ++
                "found " ++ show v ++ " instead"

-- 10. COMPOUND_STMT -> { LOCAL_DECS STATEMENT_LIST }
parseCompoundStmt :: (CompoundStmt -> [Token] -> a) -> [Token] -> a
parseCompoundStmt cb (Token T_OPEN_BRACE _ _:tokens) =
    parseLocalDecs localDecsHelper tokens where
        -- localDecsHelper :: LocalDecs -> [Token] -> a
        localDecsHelper l = parseStatementList (statementListHelper l)
        -- statementListHelper :: LocalDecs -> StatementList -> [Token] -> a
        statementListHelper l s (Token T_CLOSE_BRACE _ _:ts) =
            cb (CompoundStmt l s) ts
        statementListHelper _ _ (Token _ v l:_) =
            error $ "Line " ++ show l ++ ": " ++
                    "expected \"}\"; " ++
                    "found " ++ show v ++ " instead"
parseCompoundStmt _ (Token _ v l:_) =
    error $ "Line " ++ show l ++ ": " ++
            "expected \"{\"; " ++
            "found " ++ show v ++ " instead"

-- 11. LOCAL_DECS -> LOCAL_DECS VAR_DEC | <empty>
parseLocalDecs :: (LocalDecs -> [Token] -> a) -> [Token] -> a
parseLocalDecs cb (token:tokens)
    | isTypeSpecifier token = parseVarDec (helper []) tokens
    | otherwise = cb EmptyLocalDecs (token:tokens)
    where
        -- helper :: [VarDec] -> VarDec -> [Token] -> a
        helper acc v (t:ts)
            | isTypeSpecifier t = parseVarDec (helper (v:acc)) ts
            | otherwise = cb (LocalDecs $ reverse (v:acc)) (t:ts)

-- 12. STATEMENT_LIST -> STATEMENT_LIST STATEMENT | <empty>
parseStatementList :: (StatementList -> [Token] -> a) -> [Token] -> a
parseStatementList cb (token:tokens)
    | tokenType token == T_CLOSE_BRACKET = cb EmptyStatementList (token:tokens)
    | otherwise = parseStatement (helper []) (token:tokens)
    where
        -- helper :: [Statement] -> Statement -> [Token] -> a
        helper ss s (t:ts)
            | tokenType t == T_CLOSE_BRACKET =
                cb (StatementList $ reverse (s:ss)) ts
            | otherwise =
                parseStatement (helper (s:ss)) (t:ts)

-- 13. STATEMENT -> EXPRESSION_STMT
--                | COMPOUND_STMT
--                | IF_STMT
--                | WHILE_STMT
--                | RETURN_STMT
--                | WRITE_STMT
parseStatement :: (Statement -> [Token] -> a) -> [Token] -> a
parseStatement cb (Token T_IF _ _:ts) = parseIfStmt cb ts
parseStatement cb (Token T_WHILE _ _:ts) = parseWhileStmt cb ts
parseStatement cb (Token T_RETURN _ _:ts) = parseReturnStmt cb ts
parseStatement cb (Token T_WRITE _ _:ts) = parseWriteStmt cb ts
parseStatement cb (Token T_WRITELN _ _:ts) = parseWritelnStmt cb ts
parseStatement cb (Token T_OPEN_BRACE v l:ts) =
    parseCompoundStmt helper (Token T_OPEN_BRACE v l:ts) where
        -- helper :: CompoundStmt -> [Token] -> a
        helper c = cb (CompoundStmtStmt c)
parseStatement cb ts =
    parseExpressionStmt cb ts

-- 14. EXPRESSION_STMT -> EXPRESSION ; | ;
parseExpressionStmt :: (Statement -> [Token] -> a) -> [Token] -> a
parseExpressionStmt cb (Token T_SEMICOLON _ _:tokens) =
    cb EmptyExpressionStmt tokens
parseExpressionStmt cb tokens =
    parseExpression helper tokens where
        -- helper :: Expression -> [Token] -> a
        helper e (Token T_SEMICOLON _ _:ts) = cb (ExpressionStmt e) ts
        helper _ (Token _ v l:_) =
            error $ "Line " ++ show l ++ ": " ++
                    "expected \";\"; " ++
                    "found " ++ show v ++ " instead"

-- 15. IF_STMT -> if ( EXPRESSION ) STATEMENT
--              | if ( EXPRESSION ) STATEMENT else STATEMENT
parseIfStmt :: (Statement -> [Token] -> a) -> [Token] -> a
parseIfStmt cb (Token T_OPEN_PARENTHESIS _ _:tokens) =
    parseExpression expressionHelper tokens where
        -- expressionHelper :: Expression -> [Token] -> a
        expressionHelper e (Token T_CLOSE_PARENTHESIS _ _:ts) =
            parseStatement (ifStatementHelper e) ts
        expressionHelper _ (Token _ v l:_) =
            error $ "Line " ++ show l ++ ": " ++
                    "expected \")\"; " ++
                    "found " ++ show v ++ " instead"
        -- ifStatementHelper :: Expression -> Statement -> [Token] -> a
        ifStatementHelper e s (Token T_ELSE _ _:ts) =
            parseStatement (elseStatementHelper e s) ts
        ifStatementHelper e s ts = cb (IfStmt e s) ts
        -- elseStatementHelper :: Expression -> Statement -> Statement -> [Token] -> a
        elseStatementHelper e t f = cb (IfElseStmt e t f)
parseIfStmt _ (Token _ v l:_) =
    error $ "Line " ++ show l ++ ": " ++
            "expected \"(\"; " ++
            "found " ++ show v ++ " instead"

-- 16. WHILE_STMT -> while ( EXPRESSION ) statement
parseWhileStmt :: (Statement -> [Token] -> a) -> [Token] -> a
parseWhileStmt cb (Token T_OPEN_PARENTHESIS _ _:tokens) =
    parseExpression expressionHelper tokens where
        -- expressionHelper :: Expression -> [Token] -> a
        expressionHelper e (Token T_CLOSE_PARENTHESIS _ _:ts) =
            parseStatement (statementHelper e) ts
        expressionHelper _ (Token _ v l:_) =
            error $ "Line " ++ show l ++ ": " ++
                    "expected \")\"; " ++
                    "found " ++ show v ++ " instead"
        -- statementHelper :: Expression -> Statement -> [Token] -> a
        statementHelper e s = cb (WhileStmt e s)
parseWhileStmt _ (Token _ v l:_) =
    error $ "Line " ++ show l ++ ": " ++
            "expected \"(\"; " ++
            "found " ++ show v ++ " instead"

-- 17. RETURN_STMT -> return ; | return EXPRESSION ;
parseReturnStmt :: (Statement -> [Token] -> a) -> [Token] -> a
parseReturnStmt cb (Token T_SEMICOLON _ _:tokens) = cb EmptyReturnStmt tokens
parseReturnStmt cb tokens = parseExpression helper tokens where
    -- helper :: Expression -> [Token] -> a
    helper e (Token T_SEMICOLON _ _:ts) = cb (ReturnStmt e) ts
    helper _ (Token _ v l:_) =
        error $ "Line " ++ show l ++ ": " ++
                "expected \";\"; " ++
                "found " ++ show v ++ " instead"

-- 18. WRITE_STMT -> write ( EXRESSION ) ; | writeln ( ) ;
parseWriteStmt :: (Statement -> [Token] -> a) -> [Token] -> a
parseWriteStmt cb (Token T_OPEN_PARENTHESIS _ _:tokens) =
    parseExpression helper tokens where
        -- helper :: Expression -> [Token] -> a
        helper e (Token T_CLOSE_PARENTHESIS _ _:
                  Token T_SEMICOLON _ _:ts) =
            cb (WriteStmt e) ts
        helper _ (Token _ v l:_) =
            error $ "Line " ++ show l ++ ": " ++
                    "expected \");\"; " ++
                    "found " ++ show v ++ " instead"
parseWriteStmt _ (Token _ v l:_) =
    error $ "Line " ++ show l ++ ": " ++
            "expected \"(\"; " ++
            "found " ++ show v ++ " instead"

-- 18. WRITE_STMT -> write ( EXRESSION ) ; | writeln ( ) ;
parseWritelnStmt :: (Statement -> [Token] -> a) -> [Token] -> a
parseWritelnStmt cb (Token T_OPEN_PARENTHESIS _ _:
                     Token T_CLOSE_PARENTHESIS _ _:
                     Token T_SEMICOLON _ _:ts) =
    cb WritelnStmt ts
parseWritelnStmt _ (Token _ v l:_) =
    error $ "Line " ++ show l ++ ": " ++
            "expected \"();\"; " ++
            "found " ++ show v ++ " instead"

-- 19. EXPRESSION -> VAR = EXPRESSION | COMP_EXP
parseExpression :: (Expression -> [Token] -> a) -> [Token] -> a
parseExpression cb (Token T_IDENTIFIER i _:
                    Token T_ASSIGNMENT _ _:ts) =
    parseExpression helper ts where
        -- helper :: Expression -> [Token] -> a
        helper e = cb (AssignmentExpression (Var i RawVar) e)
parseExpression cb (Token T_IDENTIFIER i _:
                    Token T_ASTERISK _ _:ts) =
    parseExpression helper ts where
        -- helper :: Expression -> [Token] -> a
        helper e = cb (AssignmentExpression (Var i PointerVar) e)
parseExpression cb (Token T_IDENTIFIER i _:
                    Token T_OPEN_BRACKET _ _:ts) =
    parseExpression (arrayHelper i) ts where
        -- arrayHelper :: Identifier -> Expression -> [Token] -> a
        arrayHelper i e (Token T_CLOSE_BRACKET _ _:
                         Token T_ASSIGNMENT _ _:ts) =
            parseExpression (assignmentHelper i e) ts
        arrayHelper i e (Token T_CLOSE_BRACKET _ _:ts) =
            parseExpression
        arrayHelper _ _ (Token _ v l:_) =
            error $ "Line " ++ show l ++ ": " ++
                    "expected \"]\"; " ++
                    "found " ++ show v ++ " instead"
        -- assignmentHelper :: Identifier -> Expression -> [Token] -> a
        assignmentHelper i e ts =
            cb (AssignmentExpression $ Var i $ ArrayVar e)
parseExpression cb ts =
    parseCompExp helper ts where
        -- helper :: CompExp -> [Token] -> a
        helper c = cb (Expression c)

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

isTypeSpecifier :: Token -> Bool
isTypeSpecifier = (`elem` [T_INT, T_STRING, T_VOID]) . tokenType
