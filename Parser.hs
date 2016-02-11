-- Chris Egerton
-- February 3-10, 2016

--      This is the Parser module.
--      Its sole purpose is to construct a syntax tree in the form of a Program
-- from a list of Tokens.
--      It is assumed that this list of Tokens has the form of one returned by
-- a call to the tokenize function of the Scanner module--that is, that it
-- contains a stream of regular, valid tokens, followed by a single EOF token
-- repeated ad infinitum.

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
parseProgram :: [Token] -> Program
parseProgram = parseDeclarationList helper where
    -- helper :: DeclarationList -> [Token] -> Program
    helper decList _ = Program decList

-- 2. DECLARATION_LIST -> DECLARATION_LIST DECLARATION | DECLARATION
parseDeclarationList :: (DeclarationList -> [Token] -> a) -> [Token] -> a
parseDeclarationList cb = parseDeclaration (helper []) where
    -- helper :: [Declaration] -> Declaration -> [Token] -> a
    helper acc dec (Token T_END_OF_FILE v l:_) =
        cb (DeclarationList $ reverse (dec:acc)) [Token T_END_OF_FILE v l]
    helper acc dec ts =
        parseDeclaration (helper (dec:acc)) ts

-- 3. DECLARATION -> VAR_DEC | FUN_DEC
-- 4. VAR_DEC -> TYPE_SPECIFIER <id> ;
--             | TYPE_SPECIFIER *<id> ;
--             | TYPE_SPECIFIER <id>[ <num> ] ;
-- 6. FUN_DEC -> TYPE_SPECIFIER <id> ( PARAMS ) COMPOUND_STMT
parseDeclaration :: (Declaration -> [Token] -> a) -> [Token] -> a
parseDeclaration cb (token:tokens) = let t' = parseTypeSpecifier token in
    case tokens of
        -- TYPE_SPECIFIER <id> ;
        (Token T_IDENTIFIER i _:
         Token T_SEMICOLON _ _:ts) -> cb (VarDecDeclaration $ VarDec t' RawVarDec i) ts
        -- TYPE_SPECIFIER * <id> ;
        (Token T_ASTERISK _ _:
         Token T_IDENTIFIER i _:
         Token T_SEMICOLON _ _:ts) -> cb (VarDecDeclaration $ VarDec t' PointerVarDec i) ts
        -- TYPE_SPECIFIER <id> [ <num> ] ;
        (Token T_IDENTIFIER i _:
         Token T_OPEN_BRACKET _ _:
         Token T_NUMBER n _:
         Token T_CLOSE_BRACKET _ _:
         Token T_SEMICOLON _ _:ts) -> cb (VarDecDeclaration $ VarDec t' (ArrayVarDec $ parseInt n) i) ts
        -- TYPE_SPECIFIER <id> ( PARAMS ) COMPOUND_STMT
        (Token T_IDENTIFIER identifier _:
         Token T_OPEN_PARENTHESIS _ _:ts) -> parseParams (paramsHelper identifier) ts where
            -- paramsHelper :: Identifier -> Params -> [Token] -> a
            paramsHelper i p (Token T_CLOSE_PARENTHESIS _ _:ts') =
                parseCompoundStmt (compoundStmtHelper i p) ts'
            paramsHelper _ _ (Token _ v l:_) = error $
                "Line " ++ show l ++
                ": expected \")\"; " ++
                "found " ++ show v ++ " instead"
            -- compoundStmtHelper :: Identifier -> Param -> CompoundStmt -> [Token] -> a
            compoundStmtHelper i p c =
                 cb (FunDecDeclaration $ FunDec t' i p c)
        (t:_) ->  error $
            "Line " ++ show (tokenLine t) ++
            ": expected \"<vardec> or <fundec>\"; " ++
            "found " ++ show (tokenValue t) ++ " instead"

-- 4. VAR_DEC -> TYPE_SPECIFIER <id> ;
--             | TYPE_SPECIFIER *<id> ;
--             | TYPE_SPECIFIER <id>[ <num> ] ;
parseVarDec :: (VarDec -> [Token] -> a) -> [Token] -> a
parseVarDec cb (token:tokens) = helper (parseTypeSpecifier token) tokens where
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
parseTypeSpecifier :: Token -> TypeSpecifier
parseTypeSpecifier t = case tokenType t of
    T_INT -> IntType
    T_STRING -> StringType
    T_VOID -> VoidType
    _ -> error $
        "Line " ++ show (tokenLine t) ++ ": " ++
        "expected int, string, or void; " ++
        "found " ++ show (tokenValue t) ++ " instead"

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
parseParam cb (token:tokens) =
    helper (parseTypeSpecifier token) tokens where
        -- helper :: TypeSpecifier -> [Token] -> a
        helper t (Token T_IDENTIFIER i _:
                         Token T_OPEN_BRACKET _ _:
                         Token T_CLOSE_BRACKET _ _:ts) =
            cb (Param t ArrayParam i) ts
        helper t (Token T_IDENTIFIER i _:ts) =
            cb (Param t RawParam i) ts
        helper t (Token T_ASTERISK _ _:
                               Token T_IDENTIFIER i _:ts) =
            cb (Param t PointerParam i) ts
        helper _ (Token _ v l:_) =
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
    | isTypeSpecifier token = parseVarDec (helper []) (token:tokens)
    | otherwise = cb EmptyLocalDecs (token:tokens)
    where
        -- helper :: [VarDec] -> VarDec -> [Token] -> a
        helper acc v (t:ts)
            | isTypeSpecifier t = parseVarDec (helper (v:acc)) (t:ts)
            | otherwise = cb (LocalDecs $ reverse (v:acc)) (t:ts)

-- 12. STATEMENT_LIST -> STATEMENT_LIST STATEMENT | <empty>
parseStatementList :: (StatementList -> [Token] -> a) -> [Token] -> a
parseStatementList cb (token:tokens)
    | tokenType token == T_CLOSE_BRACE = cb EmptyStatementList (token:tokens)
    | otherwise = parseStatement (helper []) (token:tokens)
    where
        -- helper :: [Statement] -> Statement -> [Token] -> a
        helper ss s (t:ts)
            | tokenType t == T_CLOSE_BRACE =
                cb (StatementList $ reverse (s:ss)) (t:ts)
            | otherwise =
                parseStatement (helper (s:ss)) (t:ts)

-- 13. STATEMENT -> EXPRESSION_STMT
--                | COMPOUND_STMT
--                | IF_STMT
--                | WHILE_STMT
--                | RETURN_STMT
--                | WRITE_STMT
parseStatement :: (Statement -> [Token] -> a) -> [Token] -> a
parseStatement cb (token:tokens) = case tokenType token of
    T_IF -> parseIfStmt cb tokens
    T_WHILE -> parseWhileStmt cb tokens
    T_RETURN -> parseReturnStmt cb tokens
    T_WRITE -> parseWriteStmt cb tokens
    T_WRITELN -> parseWritelnStmt cb tokens
    T_OPEN_BRACE -> parseCompoundStmt (cb . CompoundStmtStmt) (token:tokens)
    _ -> parseExpressionStmt cb (token:tokens)

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
-- 20. VAR -> <id> | <id>[ EXPRESSION ] | *<id>
parseExpression :: (Expression -> [Token] -> a) -> [Token] -> a
parseExpression cb tokens = case tokens of
    -- <id> = Expression
    (Token T_IDENTIFIER i _:
     Token T_ASSIGNMENT _ _:ts) -> parseExpression (assignmentHelper $ Var i RawVar) ts
    -- * <id> = Expression
    (Token T_ASTERISK _ _:
     Token T_IDENTIFIER i _:ts) -> parseExpression (assignmentHelper $ Var i PointerVar) ts
    -- <id> [ => <id> [ Expression ] (=?)
    (Token T_IDENTIFIER i _:
     Token T_OPEN_BRACKET _ _:ts) -> parseExpression (arrayHelper tokens i) ts
    -- CompExp
    _ -> parseCompExp (cb . SimpleExpression) tokens
    where
        -- assignmentHelper :: Var -> Expression -> [Token] -> a
        assignmentHelper v e = cb (AssignmentExpression v e)
        -- arrayHelper :: [Token] -> Identifier -> Expression -> [Token] -> a
        arrayHelper _ i e (Token T_CLOSE_BRACKET _ _:
                           Token T_ASSIGNMENT _ _:ts) =
            parseExpression (assignmentHelper $ Var i $ ArrayVar e) ts
        arrayHelper backup _ _ (Token T_CLOSE_BRACKET _ _:_) =
            parseCompExp (cb . SimpleExpression) backup
        arrayHelper _ _ _ (t:_) = error $
            "Line " ++ show (tokenLine t) ++ ": " ++
            "expected \"<relop>\"; " ++
            "found " ++ show (tokenValue t) ++ " instead"

-- 21. COMP_EXP -> E RELOP E | E
parseCompExp :: (CompExp -> [Token] -> a) -> [Token] -> a
parseCompExp cb = parseE simpleHelper where
    -- simpleHelper :: E -> [Token] -> a
    simpleHelper e (t:ts)
        | isRelOp t = parseE (compHelper e (parseRelOp t)) ts
        | otherwise = cb (SimpleExp e) (t:ts)
    -- compHelper :: E -> RelOp -> E -> [Token] -> a
    compHelper e1 ro e2 = cb (CompExp e1 ro e2)

-- 22. RELOP -> <= | < | == | != | > | >=
parseRelOp :: Token -> RelOp
parseRelOp t = case tokenType t of
    T_LESS_THAN_OR_EQUAL -> LessThanOrEqualRelOp
    T_LESS_THAN -> LessThanRelOp
    T_EQUAL -> EqualRelOp
    T_NOT_EQUAL -> NotEqualRelOp
    T_GREATER_THAN -> GreaterThanRelOp
    T_GREATER_THAN_OR_EQUAL -> GreaterThanOrEqualRelOp
    _ -> error $
        "Line " ++ show (tokenLine t) ++ ": " ++
        "expected \"<relop>\"; " ++
        "found " ++ show (tokenValue t) ++ " instead"

-- 23. E -> E ADDOP T | T
parseE :: (E -> [Token] -> a) -> [Token] -> a
parseE cb = parseT tHelper where
    -- tHelper :: T -> [Token] -> a
    tHelper t' (t:ts)
        | isAddOp t = parseT (eHelper (SimpleE t') (parseAddOp t)) ts
        | otherwise = cb (SimpleE t') (t:ts)
    -- eHelper :: E -> AddOp -> T -> [Token] -> a
    eHelper e op t' (t:ts)
        | isAddOp t = parseT (eHelper (AddE e op t') (parseAddOp t)) ts
        | otherwise = cb (AddE e op t') (t:ts)

-- 24. ADDOP -> + | -
-- Breaking the pattern, but much easier this way.
parseAddOp :: Token -> AddOp
parseAddOp t = case tokenType t of
    T_PLUS -> PlusAddOp
    T_HYPHEN -> MinusAddOp
    _ -> error $
        "Line " ++ show (tokenLine t) ++ ": " ++
        "expected \"<addop>\"; " ++
        "found " ++ show (tokenValue t) ++ " instead"

-- 25. T -> T MULOP F | F
parseT :: (T -> [Token] -> a) -> [Token] -> a
parseT cb = parseF fHelper where
    -- fHelper :: F -> [Token] -> a
    fHelper f (t:ts)
        | isMulOp t = parseF (tHelper (SimpleT f) (parseMulOp t)) ts
        | otherwise = cb (SimpleT f) (t:ts)
    -- tHelper :: T -> MulOp -> F -> [Token] -> a
    tHelper t' mo f (t:ts)
        | isMulOp t = parseF (tHelper (MulT t' mo f) (parseMulOp t)) ts
        | otherwise = cb (MulT t' mo f) (t:ts)

-- 26. MULOP -> * | / | %
parseMulOp :: Token -> MulOp
parseMulOp t = case tokenType t of
    T_ASTERISK -> TimesMulOp
    T_SLASH -> DivMulOp
    T_PERCENT -> ModMulOp
    _ -> error $
        "Line " ++ show (tokenLine t) ++ ": " ++
        "expected \"<mulop>\"; " ++
        "found " ++ show (tokenValue t) ++ " instead"

-- 27. F -> -F | &Factor | *Factor | Factor
parseF :: (F -> [Token] -> a) -> [Token] -> a
parseF cb (t:ts) = case tokenType t of
    T_HYPHEN -> parseF (cb . NegativeF) ts
    T_AMPERSAND -> parseFactor (cb . ReferenceF) ts
    T_ASTERISK -> parseFactor (cb . DereferenceF) ts
    _ -> parseFactor (cb . SimpleF) (t:ts)

-- 28. Factor -> ( EXPRESSION ) |
                --  FUN_CALL |
                --  read ( ) |
                --  *<id> |
                --  <id> |
                --  <id>[EXPRESSION] |
                --  <num> |
                --  <string>
parseFactor :: (Factor -> [Token] -> a) -> [Token] -> a
parseFactor cb tokens = case tokens of
    -- read ( )
    (Token T_READ _ _:
     Token T_OPEN_PARENTHESIS _ _:
     Token T_CLOSE_PARENTHESIS _ _:ts) -> cb ReadFactor ts
    -- * <id>
    (Token T_ASTERISK _ _:
     Token T_IDENTIFIER i _:ts) -> cb (DereferenceFactor i) ts
    -- <id> [ => <id> [ Expression ]
    (Token T_IDENTIFIER i _:
     Token T_OPEN_BRACKET _ _:ts) -> parseExpression (arrayHelper i) ts
    -- <id> ( => FUN_CALL = <id> ( ARGS )
    (Token T_IDENTIFIER _ _:
     Token T_OPEN_PARENTHESIS _ _:_) -> parseFunCall funCallHelper tokens
    -- <id>
    (Token T_IDENTIFIER i _:ts) -> cb (VarFactor i) ts
    -- ( => ( Expression )
    (Token T_OPEN_PARENTHESIS _ _:ts) -> parseExpression groupedHelper ts
    -- <num>
    (Token T_NUMBER n _:ts) -> cb (NumberFactor $ parseInt n) ts
    -- <string>
    (Token T_STRING_LITERAL s _:ts) -> cb (StringFactor s) ts
    -- Something's not right...
    (Token _ v l:_) -> error $
        "Line " ++ show l ++ ": " ++
        "expected <factor>; " ++
        "found " ++ show v ++ " instead"
    where
        -- arrayHelper :: Identifier -> Expression -> [Token] -> a
        arrayHelper i e (t:ts) = case tokenType t of
            T_CLOSE_BRACKET -> cb (ArrayReferenceFactor i e) ts
            _ -> error $
                "Line " ++ show (tokenLine t) ++ ": " ++
                "expected \"]\"; " ++
                "found " ++ show (tokenValue t) ++ " instead"
        -- funCallHelper :: FunCall -> [Token] -> a
        funCallHelper = cb . FunCallFactor
        -- groupedHelper :: Expression -> [Token] -> a
        groupedHelper e (t:ts)
            | tokenType t == T_CLOSE_PARENTHESIS = cb (GroupedFactor e) ts
            | otherwise = error $
                "Line " ++ show (tokenLine t) ++ ": " ++
                "expected \")\"; " ++
                "found " ++ show (tokenValue t) ++ " instead"

-- 29. FUN_CALL -> <id> ( ARGS )
parseFunCall :: (FunCall -> [Token] -> a) -> [Token] -> a
parseFunCall cb (Token T_IDENTIFIER identifier _:
                 Token T_OPEN_PARENTHESIS _ _:tokens) =
    parseArgs (helper identifier) tokens where
        -- helper :: Identifier -> Args -> [Token] -> a
        helper i a (Token T_CLOSE_PARENTHESIS _ _:ts) =
            cb (FunCall i a) ts
        helper _ _ (Token _ v l:_) =
            error $ "Line " ++ show l ++ ": " ++
                    "expected \")\"; " ++
                    "found " ++ show v ++ " instead"
parseFunCall _ (Token _ v l:_) =
    error $ "Line " ++ show l ++ ": " ++
            "expected \"<id>(\"; " ++
            "found " ++ show v ++ " instead"

-- 30. ARGS -> ARG_LIST | <empty>
parseArgs :: (Args -> [Token] -> a) -> [Token] -> a
parseArgs cb (Token T_CLOSE_PARENTHESIS v l:ts) =
    cb EmptyArgs (Token T_CLOSE_PARENTHESIS v l:ts)
parseArgs cb ts = parseArgList helper ts where
    -- helper :: ArgList -> [Token] -> a
    helper = cb . Args

-- 31. ARG_LIST -> ARG_LIST , EXPRESSION | EXPRESSION
parseArgList :: (ArgList -> [Token] -> a) -> [Token] -> a
parseArgList cb = parseExpression (helper []) where
    -- helper :: [Expression] -> Expression -> [Token] -> a
    helper acc e (Token T_COMMA _ _:ts) = parseExpression (helper (e:acc)) ts
    helper acc e ts = cb (ArgList $ reverse (e:acc)) ts

parseInt :: String -> Int
parseInt = read

isTypeSpecifier :: Token -> Bool
isTypeSpecifier = (`elem` [T_INT, T_STRING, T_VOID]) . tokenType

isRelOp :: Token -> Bool
isRelOp = (`elem`
    [T_LESS_THAN_OR_EQUAL, T_LESS_THAN,
     T_EQUAL, T_NOT_EQUAL,
     T_GREATER_THAN, T_GREATER_THAN_OR_EQUAL]) . tokenType

isAddOp :: Token -> Bool
isAddOp = (`elem` [T_PLUS, T_HYPHEN]) . tokenType

isMulOp :: Token -> Bool
isMulOp = (`elem` [T_ASTERISK, T_SLASH, T_PERCENT]) . tokenType
