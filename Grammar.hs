-- Chris Egerton
-- February 3-10, 2016

--      This is the Grammar module.
--      Its sole purpose is to provide definitions for datatypes used in
-- representing the grammar of BPL programmatically.

module Grammar where

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

import Token

type Identifier = String
data VarDecMetaType =
    RawVarDec |
    PointerVarDec |
    ArrayVarDec Int
    deriving (Eq, Show)
data ParamMetaType =
    RawParam |
    PointerParam |
    ArrayParam
    deriving (Eq, Show)
data VarMetaType =
    RawVar |
    PointerVar |
    ArrayVar Expression
    deriving (Eq, Show)

-- 1. PROGRAM -> DECLARATION_LIST
data Program =
    Program DeclarationList
    deriving (Eq, Show)
-- 2. DECLARATION_LIST -> DECLARATION_LIST DECLARATION | DECLARATION
data DeclarationList =
    DeclarationList [Declaration]
    deriving (Eq, Show)
-- 3. DECLARATION -> VAR_DEC | FUN_DEC
data Declaration =
    VarDecDeclaration VarDec | FunDecDeclaration FunDec
    deriving (Eq, Show)
-- 4. VAR_DEC -> TYPE_SPECIFIER <id> ;
--             | TYPE_SPECIFIER *<id> ;
--             | TYPE_SPECIFIER <id>[ <num> ] ;
data VarDec =
    VarDec TypeSpecifier VarDecMetaType Identifier
    deriving (Eq, Show)
-- 5. TYPE_SPECIFIER -> int | void | string
data TypeSpecifier =
    IntType | VoidType | StringType
    deriving (Eq, Show)
-- 6. FUN_DEC -> TYPE_SPECIFIER <id> ( PARAMS ) COMPOUND_STMT
data FunDec =
    FunDec TypeSpecifier Identifier Params CompoundStmt
    deriving (Eq, Show)
-- 7. PARAMS -> void | PARAM_LIST
data Params =
    Params ParamList | EmptyParams
    deriving (Eq, Show)
-- 8. PARAM_LIST -> PARAM_LIST , PARAM | PARAM
data ParamList =
    ParamList [Param]
    deriving (Eq, Show)
-- 9. PARAM -> TYPE_SPECIFIER <id>
--           | TYPE_SPECIFIER *<id>
--           | TYPE_SPECIFIER <id>[ ]
data Param =
    Param TypeSpecifier ParamMetaType Identifier
    deriving (Eq, Show)
-- 10. COMPOUND_STMT -> { LOCAL_DECS STATEMENT_LIST }
data CompoundStmt =
    CompoundStmt LocalDecs StatementList
    deriving (Eq, Show)
-- 11. LOCAL_DECS -> LOCAL_DECS VAR_DEC | <empty>
data LocalDecs =
    LocalDecs [VarDec] | EmptyLocalDecs
    deriving (Eq, Show)
-- 12. STATEMENT_LIST -> STATEMENT_LIST STATEMENT | <empty>
data StatementList =
    StatementList [Statement] | EmptyStatementList
    deriving (Eq, Show)
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
data Statement =
    ExpressionStmt Expression | EmptyExpressionStmt |
    CompoundStmtStmt CompoundStmt |
    IfStmt Expression Statement | IfElseStmt Expression Statement Statement |
    WhileStmt Expression Statement |
    ReturnStmt Expression | EmptyReturnStmt |
    WriteStmt Expression | WritelnStmt
    deriving (Eq, Show)
-- -- 19. EXPRESSION -> VAR = EXPRESSION | COMP_EXP
data Expression =
    AssignmentExpression Var Expression | CompExpression CompExp
    deriving (Eq, Show)
-- 20. VAR -> <id> | <id>[ EXPRESSION ] | *<id>
data Var =
    Var Identifier VarMetaType
    deriving (Eq, Show)
-- 21. COMP_EXP -> E RELOP E | E
data CompExp =
    CompExp E RelOp E | SimpleExp E
    deriving (Eq, Show)
-- 22. RELOP -> <= | < | == | != | > | >=
data RelOp =
    LessThanOrEqualRelOp |
    LessThanRelOp |
    EqualRelOp |
    NotEqualRelOp |
    GreaterThanRelOp |
    GreaterThanOrEqualRelOp
    deriving (Eq, Show)
-- 23. E -> E ADDOP T | T
data E =
    AddE E AddOp T | SimpleE T
    deriving (Eq, Show)
-- 24. ADDOP -> + | -
data AddOp =
    PlusAddOp | MinusAddOp
    deriving (Eq, Show)
-- 25. T -> T MULOP F | F
data T =
    MulT T MulOp F | SimpleT F
    deriving (Eq, Show)
-- 26. MULOP -> * | / | %
data MulOp =
    TimesMulOp | DivMulOp | ModMulOp
    deriving (Eq, Show)
-- 27. F -> -F | &Factor | *Factor | Factor
data F =
    NegativeF F | ReferenceF Factor | DereferenceF Factor | SimpleF Factor
    deriving (Eq, Show)
-- 28. Factor -> ( EXPRESSION ) |
                --  FUN_CALL |
                --  read ( ) |
                --  *<id> |
                --  <id> |
                --  <id>[EXPRESSION] |
                --  <num> |
                --  <string>
data Factor =
    GroupedFactor Expression |
    FunCallFactor FunCall |
    ReadFactor |
    DereferenceFactor Identifier |
    VarFactor Identifier |
    ArrayReferenceFactor Identifier Expression |
    NumberFactor Int |
    StringFactor String
    deriving (Eq, Show)
-- 29. FUN_CALL -> <id> ( ARGS )
data FunCall =
    FunCall Identifier Args
    deriving (Eq, Show)
-- 30. ARGS -> ARG_LIST | <empty>
data Args =
    Args ArgList | EmptyArgs
    deriving (Eq, Show)
-- 31. ARG_LIST -> ARG_LIST , EXPRESSION | EXPRESSION
data ArgList =
    ArgList [Expression]
    deriving (Eq, Show)


-- data Statement =
--     ExpressionStmtStatement ExpressionStmt |
--     CompoundStmtStatement CompoundStmt |
--     IfStmtStatement IfStmt |
--     WhileStmfStatement WhileStmt |
--     ReturnStmfStatement ReturnStmt |
--     WriteStmfStatement WriteStmt
--
-- data ExpressionStmt = ExpressionStmt Expression | EmptyExpressionStmt
--
-- data IfStmt = IfStmt Expression Statement |
--               IfElseStmt Expression Statement Statement
--
-- data WhileStmt = WhileStmt Expression Statement
--
-- data ReturnStmt = ReturnStmt Expression | EmptyReturnStmt
--
-- data WriteStmt = WriteStmt Expression | WritelnStmt
