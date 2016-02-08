-- Chris Egerton
-- February 3, 2016

--      This is the Grammar module.
--      Its sole purpose is to provide definitions for datatypes used in
-- representing the grammar of BPL programmatically.

module Grammar where

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
    AssignmentExpression Var Expression | Expression CompExp
    deriving (Eq, Show)
-- 20. VAR -> <id> | <id>[ EXPRESSION ] | *<id>
data Var =
    Var Identifier VarMetaType
    deriving (Eq, Show)
-- 21. COMP_EXP -> E RELOP E | E
data CompExp =
    CompExp E Relop E | SimpleExp E
    deriving (Eq, Show)
-- 22. RELOP -> <= | < | == | != | > | >=
data Relop =
    LessThanOrEqualRelop |
    LessThanRelop |
    EqualRelop |
    NotEqualRelop |
    GreaterThanRelop |
    GreaterThanOrEqualRelop
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
    ExpressionFactor Expression |
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
