-- Chris Egerton
-- March - April, 2016

--      This is the Analyzer module.
--      It serves several purposes. Firstly, to verify that the program is
-- type-safe. Also, to return a new Program tree (from Types.hs, not
-- Grammar.hs), which contains some consolidated nodes and type information for
-- everything from an Expression downward. Next, it checks for duplicate
-- global declarations (ones that share an identifier) and throws an error if
-- any are found. Finally, it makes sure that the program has a declaration for
-- a function named "main" of type void which takes no arguments.
--      If the debugging variable is set to True, information about these type
-- assignments will be logged as it is discovered, as well as information
-- regarding the assignment of types to scoped and global variables and
-- functions.

module Analyzer where

import qualified Grammar
import Types
import Debug.Trace (trace)

debugging :: Bool
debugging = False

debug :: String -> b -> b
debug info result
    | debugging = trace info result
    | otherwise = result

logAssignment :: NodeType -> String -> LineNumber -> NodeType
logAssignment n c l = debug message n where
    message = "Line " ++ show l ++ ": " ++ c ++ " node assigned type " ++ showNode n

splitScan :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
splitScan f base l = (single, reverse list) where
    (single, list) = helper base l []
    helper b [] acc = (b, acc)
    helper b (x:xs) acc = helper next xs (result:acc) where
        (next, result) = f b x

processProgram :: Grammar.Program -> Program
processProgram (Grammar.Program (Grammar.DeclarationList ds _) _) = verifiedResult where
    (s, tds) = splitScan programHelper emptyScope ds
    verifiedResult = mainVerifiedProgram s tds

mainVerifiedProgram :: Scope -> [Declaration] -> Program
mainVerifiedProgram s tds = result where
    mainDec = if Types.contains s "main"
        then Types.lookup s "main" 1
        else error "Programs must contain a main function"
    mainFun = case mainDec of
        VarDecType _ -> error "Declaration for main must correspond to a function"
        FunDecType f -> f
    result = if funReturn mainFun == VoidFun
        then
            if null $ funArgs mainFun
                then Program tds
                else error "Main function cannot take any arguments"
        else
            error "Main function must have type void"

programHelper :: Scope -> Grammar.Declaration -> (Scope, Declaration)
programHelper s d = case d of
    (Grammar.VarDecDeclaration v _) -> (newScope, newDec) where
        newScope = addToScope s (extractVarName v) (VarDecType varType)
        newDec = processDeclaration newScope d
        varType = silentlyExtractVarType v
    (Grammar.FunDecDeclaration f _) -> (newScope, newDec) where
        newScope = addFunToScope s f
        newDec = processDeclaration newScope d

processDeclaration :: Scope -> Grammar.Declaration -> Declaration
processDeclaration _ (Grammar.VarDecDeclaration v _) = result where
    result = VarDec (extractVarName v) (extractVarType v)
processDeclaration s (Grammar.FunDecDeclaration (Grammar.FunDec t i p c l) _) = verifiedResult where
    params = processParams p
    newScope = addParamsToScope s (Grammar.FunDec t i p c l)
    returnType = extractFunReturnType t
    body = processCompoundStmt newScope (funNodeType (returnType, [])) c
    verifiedResult = returnVerifiedFunction i returnType params body

--  If the function is non-void, guarantee (erring on the side of caution) that
-- it contains a return statement, and return the resulting FunDec. Otherwise,
-- check to see if the function contains a return statement, and if not, simply
-- append one to the end of its compound statement's statement list.
returnVerifiedFunction :: Identifier -> FunReturnType -> [(Identifier, VarType)] -> CompoundStmt -> Declaration
returnVerifiedFunction i r ps c = verifiedResult where
    result = FunDec i r ps c
    safeVoidResult = FunDec i r ps safeVoidBody
    safeVoidBody = case c of
        (CompoundStmt lds ss) -> CompoundStmt lds (ss ++ [EmptyReturnStmt])
    verifiedResult = case r of
        VoidFun -> if statementHasReturn (CompoundStmtStmt c)
            then result
            else safeVoidResult
        _ -> if statementHasReturn (CompoundStmtStmt c)
            then result
            else error $
                "Non-void function " ++ i ++ " may fail to execute a return statement"

statementHasReturn :: Statement -> Bool
statementHasReturn (EmptyReturnStmt) = True
statementHasReturn (ReturnStmt _) = True
statementHasReturn (IfElseStmt _ s s') = statementHasReturn s && statementHasReturn s'
statementHasReturn (CompoundStmtStmt (CompoundStmt _ ss)) = any statementHasReturn ss
statementHasReturn _ = False
--  IfStmt and WhileStmt must both be False, since there is no guarantee that
-- their bodies will ever be executed.

processParams :: Grammar.Params -> [(Identifier, VarType)]
processParams (Grammar.EmptyParams _) = []
processParams (Grammar.Params (Grammar.ParamList ps _) _) = zip is vs where
    is = map extractParamName ps
    vs = map silentlyExtractParamType ps

processCompoundStmt :: Scope -> NodeType -> Grammar.CompoundStmt -> CompoundStmt
processCompoundStmt _ _ (Grammar.CompoundStmt _ (Grammar.EmptyStatementList _) _) =
    CompoundStmt (LocalDecs []) []
processCompoundStmt s n (Grammar.CompoundStmt l (Grammar.StatementList ss _) _) =
    CompoundStmt localDecs statements where
        localDecs = processLocalDecs l
        decList = case localDecs of
            (LocalDecs ds) -> ds
        statements = map (processStatement newScope n) ss
        newScope = foldl addVarToScope s decList
        addVarToScope s' (i, v) = addToScope s' i (VarDecType v)

processLocalDecs :: Grammar.LocalDecs -> LocalDecs
processLocalDecs (Grammar.EmptyLocalDecs _) = LocalDecs []
processLocalDecs (Grammar.LocalDecs vs _) = LocalDecs (map processLocalVarDec vs)

processLocalVarDec :: Grammar.VarDec -> (Identifier, VarType)
processLocalVarDec v = (i, t) where
    i = extractVarName v
    t = extractVarType v

processStatement :: Scope -> NodeType -> Grammar.Statement -> Statement
processStatement s _ (Grammar.ExpressionStmt e _) =
    ExpressionStmt (processExpression s e)
processStatement _ _ (Grammar.EmptyExpressionStmt _) =
    EmptyExpressionStmt
processStatement s n (Grammar.CompoundStmtStmt c _) =
    CompoundStmtStmt (processCompoundStmt s n c)
processStatement s n (Grammar.IfStmt e s' line) =
    verifiedResult where
        e' = processExpression s e
        s'' = processStatement s n s'
        verifiedResult = case nodeType e' of
            (IntNode, RawNode) -> IfStmt e' s''
            _ -> error $
                "Line " ++ show line ++ ": clause of if-statement must be of type raw integer"
processStatement s n (Grammar.IfElseStmt e s' s'' line) =
    verifiedResult where
        e' = processExpression s e
        s''' = processStatement s n s'
        s'''' = processStatement s n s''
        verifiedResult = case nodeType e' of
            (IntNode, RawNode) -> IfElseStmt e' s''' s''''
            _ -> error $
                "Line " ++ show line ++ ": clause of if-else-statement must be of type raw integer"
processStatement s n (Grammar.WhileStmt e s' line) =
    verifiedResult where
        e' = processExpression s e
        s'' = processStatement s n s'
        verifiedResult = case nodeType e' of
            (IntNode, RawNode) -> WhileStmt e' s''
            _ -> error $
                "Line " ++ show line ++ ": clause of while-statement must be of type raw integer"
processStatement s n (Grammar.ReturnStmt e line) =
    verifiedResult where
        e' = processExpression s e
        verifiedResult = if nodeType e' == n
            then
                if nodeRaw n /= VoidNode
                    then ReturnStmt e'
                    else error $
                        "Line " ++ show line ++ ": void function cannot have non-empty return statement"
                else error $
                    "Line " ++ show line ++ ": return type does not match function type"
processStatement _ n (Grammar.EmptyReturnStmt line) =
    verifiedResult where
        verifiedResult = if nodeRaw n == VoidNode
            then EmptyReturnStmt
            else error $
                "Line " ++ show line ++ ": non-void function cannot have empty return statement"
processStatement s _ (Grammar.WriteStmt e line) =
    verifiedResult where
        e' = processExpression s e
        verifiedResult = case nodeType e' of
            (IntNode, RawNode) -> WriteStmt e'
            (StringNode, RawNode) -> WriteStmt e'
            _ -> error $
                "Line " ++ show line ++ ": arguments to write statement can only be of type raw integer or raw string"
processStatement _ _ (Grammar.WritelnStmt _) =
    WritelnStmt

processExpression :: Scope -> Grammar.Expression -> Expression
processExpression s (Grammar.AssignmentExpression (Grammar.Var i m l) e line) =
    AssignmentExpression i a e' loggedNode where
        dec = Types.lookup s i l
        var = case dec of
            VarDecType v -> v
            FunDecType _ -> error $
                "Line " ++ show line ++ ": cannot assign to function "  ++ i
        -- The varType of the left-hand side
        alteredVar = case m of
            Grammar.RawVar -> var
            Grammar.PointerVar -> case varMeta var of
                PointerVar -> (varRaw var, RawVar)
                RawVar -> error $
                    "Line " ++ show line ++ ": cannot assign to address of raw variable " ++ i
                (ArrayVar _) -> error $
                    "Line " ++ show line ++ ": cannot assign to address of array variable " ++ i
            Grammar.ArrayVar _ -> case varMeta var of
                (ArrayVar _) -> (varRaw var, RawVar)
                RawVar -> error $
                    "Line " ++ show line ++ ": cannot assign to index of raw variable " ++ i
                PointerVar -> error $
                    "Line " ++ show line ++ ": cannot assign to index of pointer variable " ++ i
        e' = processExpression s e
        -- The kind of assignment that's occurring
        a = case m of
            Grammar.RawVar -> case varMeta var of
                ArrayVar _ -> error $
                    "Line " ++ show line ++ ": cannot perform raw assignment on arrays"
                _ -> RawAssignment
            Grammar.PointerVar -> DereferenceAssignment
            Grammar.ArrayVar e'' -> let index = processExpression s e'' in
                if nodeType index == (IntNode, RawNode)
                    then ArrayAssignment index
                    else error $
                        "Line " ++ show line ++ ": array indices must be of type raw integer"
        node = nodeType e'
        verifiedNode = if node == varNodeType alteredVar
            then node
            else error $
                "Line " ++ show line ++ ": types of left- and right-hand sides do not match"
        loggedNode = logAssignment verifiedNode "Expression" line
processExpression s (Grammar.SimpleExpression c line) =
    SimpleExpression compExp loggedNode where
        compExp = processCompExp s c
        node = nodeType compExp
        loggedNode = logAssignment node "Expression" line

processCompExp :: Scope -> Grammar.CompExp -> CompExp
processCompExp s (Grammar.CompExp e r e' line) =
    CompExp e'' r' e''' loggedNode where
        e'' = processE s e
        r' = processRelOp r
        e''' = processE s e'
        rawInt = (IntNode, RawNode)
        verifiedNode = if nodeType e'' == rawInt && nodeType e''' == rawInt
            then rawInt
            else error $
                "Line " ++ show line ++ ": both operands of CompExp must be of type raw integer"
        loggedNode = logAssignment verifiedNode "CompExp" line
processCompExp s (Grammar.SimpleExp e line) =
    SimpleExp e' loggedNode where
        e' = processE s e
        node = nodeType e'
        loggedNode = logAssignment node "CompExp" line

processE :: Scope -> Grammar.E -> E
processE s (Grammar.AddE e a t line) =
    AddE e' a' t' loggedNode where
        e' = processE s e
        a' = processAddOp a
        t' = processT s t
        rawInt = (IntNode, RawNode)
        verifiedNode = if nodeType e' == rawInt && nodeType t' == rawInt
            then rawInt
            else error $
                "Line " ++ show line ++ ": both operands of AddE must be of type raw integer"
        loggedNode = logAssignment verifiedNode "E" line
processE s (Grammar.SimpleE t line) =
    SimpleE t' loggedNode where
        t' = processT s t
        node = nodeType t'
        loggedNode = logAssignment node "E" line

processT :: Scope -> Grammar.T -> T
processT s (Grammar.MulT t m f line) =
    MulT t' m' f' loggedNode where
        t' = processT s t
        m' = processMulOp m
        f' = processF s f
        rawInt = (IntNode, RawNode)
        verifiedNode = if nodeType t' == rawInt && nodeType f' == rawInt
            then rawInt
            else error $
                "Line " ++ show line ++ ": both operands of MulT must be of type raw integer"
        loggedNode = logAssignment verifiedNode "T" line
processT s (Grammar.SimpleT f line) =
    SimpleT f' loggedNode where
        f' = processF s f
        node = nodeType f'
        loggedNode = logAssignment node "T" line

processF :: Scope -> Grammar.F -> F
processF s (Grammar.NegativeF f line) =
    result where
        f' = processF s f
        node = nodeType f'
        verifiedNode = case node of
            (IntNode, RawNode) -> node
            _ -> error $
                "Line " ++ show line ++ ": negative F must have child of type raw integer"
        loggedNode = logAssignment verifiedNode "F" line
        result = NegativeF f' loggedNode
processF s (Grammar.ReferenceF factor line) =
    result where
        lvalue = processLValue s factor
        node = nodeType lvalue
        verifiedNode = case node of
            (r, RawNode) -> (r, PointerNode)
            (_, PointerNode) -> error $
                "Line " ++ show line ++ ": cannot reference pointer factor"
            (_, ArrayNode) -> error $
                "Line " ++ show line ++ ": cannot reference array factor"
        loggedNode = logAssignment verifiedNode "F" line
        result = ReferenceF lvalue loggedNode
processF s (Grammar.DereferenceF factor line) =
    result where
        factor' = processFactor s factor
        node = nodeType factor'
        verifiedNode = case node of
            (r, PointerNode) -> (r, RawNode)
            (_, RawNode) -> error $
                "Line " ++ show line ++ ": cannot dereference raw factor"
            (_, ArrayNode) -> error $
                "Line " ++ show line ++ ": cannot dereference array factor"
        loggedNode = logAssignment verifiedNode "F" line
        result = DereferenceF factor' loggedNode
processF s (Grammar.SimpleF factor line) =
    SimpleF factor' loggedNode where
        factor' = processFactor s factor
        loggedNode = logAssignment (nodeType factor') "F" line

processFactor :: Scope -> Grammar.Factor -> Factor
processFactor s (Grammar.GroupedFactor e line) =
    GroupedFactor expression loggedNode where
        expression = processExpression s e
        loggedNode = logAssignment (nodeType expression) "Factor" line
processFactor s (Grammar.FunCallFactor f line) =
    FunCallFactor funCall loggedNode where
        funCall = processFunCall s f
        loggedNode = logAssignment (nodeType funCall) "Factor" line
processFactor _ (Grammar.ReadFactor line) = result where
    loggedNode = logAssignment (IntNode, RawNode) "Factor" line
    result = ReadFactor loggedNode
processFactor s (Grammar.DereferenceFactor i line) =
    VarFactor i loggedNode where
        dec = Types.lookup s i line
        var = case dec of
            VarDecType v -> v
            FunDecType _ -> error $
                "Line " ++ show line ++ ": cannot dereference function " ++ i
        ptr = case var of
            (r, PointerVar) -> (r, RawVar)
            (_, RawVar) -> error $
                "Line " ++ show line ++ ": cannot dereference raw variable " ++ i
            (_, (ArrayVar _)) -> error $
                "Line " ++ show line ++ ": cannot dereference array variable " ++ i
        loggedNode = logAssignment (varNodeType ptr) "Factor" line
processFactor s (Grammar.VarFactor i line) =
    VarFactor i loggedNode where
        dec = Types.lookup s i line
        var = case dec of
            VarDecType v -> v
            FunDecType _ -> error $
                "Line " ++ show line ++ ": cannot reference function " ++ i ++ " without calling it"
        loggedNode = logAssignment (varNodeType var) "Factor" line
processFactor s (Grammar.ArrayReferenceFactor i e line) =
    ArrayReferenceFactor i verifiedIndex loggedNode where
        index = processExpression s e
        verifiedIndex = if nodeType index == (IntNode, RawNode)
            then index
            else error $
                "Line " ++ show line ++ ": array indices must be of type raw integer"
        dec = Types.lookup s i line
        var = case dec of
            VarDecType v -> v
            FunDecType _ -> error $
                "Line " ++ show line ++ ": cannot index function " ++ i
        arr = case var of
            (a, (ArrayVar _)) -> a
            (_, RawVar) -> error $
                "Line " ++ show line ++ ": cannot index raw variable " ++ i
            (_, PointerVar) -> error $
                "Line " ++ show line ++ ": cannot index pointer variable " ++ i
        loggedNode = logAssignment (varNodeType (arr, RawVar)) "Factor" line
processFactor _ (Grammar.NumberFactor n line) = result where
    loggedNode = logAssignment (IntNode, RawNode) "Factor" line
    result = NumberFactor n loggedNode
processFactor _ (Grammar.StringFactor s' line) = result where
    loggedNode = logAssignment (StringNode, RawNode) "Factor" line
    result = StringFactor s' loggedNode

processLValue :: Scope -> Grammar.Factor -> LValue
processLValue s (Grammar.VarFactor i line) = result where
    result = RawLValue i loggedNode
    dec = Types.lookup s i line
    var = case dec of
        (VarDecType v) -> v
        (FunDecType _) -> error $
            "Line " ++ show line ++ ": cannot reference function"
    node = case var of
        (r, RawVar) -> varNodeType (r, RawVar)
        (_, PointerVar) -> error $
            "Line " ++ show line ++ ": cannot reference pointer variable"
        (_, ArrayVar _) -> error $
            "Line " ++ show line ++ ": cannot reference array variable"
    loggedNode = logAssignment node "LValue" line
processLValue s (Grammar.DereferenceFactor i line) = result where
    result = PointerLValue i loggedNode
    dec = Types.lookup s i line
    var = case dec of
        (VarDecType v) -> v
        (FunDecType _) -> error $
            "Line " ++ show line ++ ": cannot reference function"
    node = case var of
        (r, PointerVar) ->  varNodeType (r, RawVar)
        (_, RawVar) -> error $
            "Line " ++ show line ++ ": cannot dereference raw variable"
        (_, ArrayVar _) -> error $
            "Line " ++ show line ++ ": cannot dereference array variable"
    loggedNode = logAssignment node "LValue" line
processLValue s (Grammar.ArrayReferenceFactor i e line) = verifiedResult where
    e' = processExpression s e
    dec = Types.lookup s i line
    var = case dec of
        (VarDecType v) -> v
        (FunDecType _) -> error $
            "Line " ++ show line ++ ": cannot reference function"
    node = case var of
        (r, ArrayVar _) -> varNodeType (r, RawVar)
        (_, RawVar) -> error $
            "Line " ++ show line ++ ": cannot index raw variable"
        (_, PointerVar) -> error $
            "Line " ++ show line ++ ": cannot index pointer variable"
    loggedNode = logAssignment node "LValue" line
    verifiedResult = case nodeType e' of
        (IntNode, RawNode) -> ArrayLValue i e' loggedNode
        _ -> error $
            "Line " ++ show line ++ ": array indices must be of type raw integer"
processLValue s (Grammar.GroupedFactor e _) = extractLValueFromExpression s e
processLValue _ f = error $ "Line " ++ show line ++ ": cannot reference non-lvalue" where
    line = extractLineFromFactor f

processFunCall :: Scope -> Grammar.FunCall -> FunCall
processFunCall s (Grammar.FunCall i (Grammar.EmptyArgs _) line) =
    FunCall i [] loggedReturnType where
        dec = Types.lookup s i line
        fun = case dec of
            VarDecType _ -> error $ "Line " ++ show line ++ ": cannot call variable " ++ i
            FunDecType f -> f
        verifiedReturnType =
            if null $ funArgs fun
                then funNodeType fun
                else error $
                    "Line " ++ show line ++ ": function " ++ i ++ " given invalid arguments"
        loggedReturnType = logAssignment verifiedReturnType "FunCall" line
processFunCall s (Grammar.FunCall i (Grammar.Args (Grammar.ArgList es _) _) line) =
    FunCall i args loggedReturnType where
        args = map (processExpression s) es
        argTypes = map nodeType args
        dec = Types.lookup s i line
        fun = case dec of
            VarDecType _ -> error $ "Line " ++ show line ++ ": cannot call variable " ++ i
            FunDecType f -> f
        paramTypes = map varNodeType (funArgs fun)
        verifiedReturnType =
            if argTypes == paramTypes
                then funNodeType fun
                else error $
                    "Line " ++ show line ++ ": function " ++ i ++ " given invalid arguments"
        loggedReturnType = logAssignment verifiedReturnType "FunCall" line

--------------------------------------------------------------------------------

extractVarName :: Grammar.VarDec -> Identifier
extractVarName (Grammar.VarDec _ _ result _) = result

extractVarLine :: Grammar.VarDec -> Grammar.LineNumber
extractVarLine (Grammar.VarDec _ _ _ result) = result

extractVarType :: Grammar.VarDec -> VarType
extractVarType (Grammar.VarDec t m i l) = result where
    var = (rawType, metaType)
    debugMessage = "Line " ++ show l ++ ": variable " ++ i ++ " assigned type " ++ showVar var
    result = debug debugMessage var
    rawType = extractRawVarType t i l
    metaType = extractMetaVarType m

silentlyExtractVarType :: Grammar.VarDec -> VarType
silentlyExtractVarType (Grammar.VarDec t m i l) = result where
    result = (rawType, metaType)
    rawType = extractRawVarType t i l
    metaType = extractMetaVarType m

extractRawVarType :: Grammar.TypeSpecifier -> Identifier -> Grammar.LineNumber -> VarRawType
extractRawVarType (Grammar.IntType _) _ _ = IntVar
extractRawVarType (Grammar.StringType _) _ _ = StringVar
extractRawVarType (Grammar.VoidType _) i l =
    error $ "Cannot assign void type to variable " ++ i ++ " declared at line " ++ show l

extractMetaVarType :: Grammar.VarDecMetaType -> VarMetaType
extractMetaVarType Grammar.RawVarDec = RawVar
extractMetaVarType Grammar.PointerVarDec = PointerVar
extractMetaVarType (Grammar.ArrayVarDec n) = ArrayVar n

addFunToScope :: Scope -> Grammar.FunDec -> Scope
addFunToScope s f = addToScope s i t where
    i = extractFunName f
    t = FunDecType $ extractFunType f

extractFunName :: Grammar.FunDec -> Identifier
extractFunName (Grammar.FunDec _ result _ _ _) = result

extractFunBody :: Grammar.FunDec -> Grammar.CompoundStmt
extractFunBody (Grammar.FunDec _ _ _ result _) = result

extractFunType :: Grammar.FunDec -> FunType
extractFunType (Grammar.FunDec t i p _ l) = (returnType t, paramTypes p) where
    returnType (Grammar.IntType _) =
        debug ("Line " ++ show l ++ ": function " ++ i ++ " assigned return type integer") IntFun
    returnType (Grammar.StringType _) =
        debug ("Line " ++ show l ++ ": function " ++ i ++ " assigned return type string") StringFun
    returnType (Grammar.VoidType _) =
        debug ("Line " ++ show l ++ ": function " ++ i ++ " assigned return type void") VoidFun
    paramTypes (Grammar.EmptyParams _) = []
    paramTypes (Grammar.Params (Grammar.ParamList p' _) _) = map (extractVar . extractParamType) p'
    extractVar (VarDecType v) = v

extractFunReturnType :: Grammar.TypeSpecifier -> FunReturnType
extractFunReturnType (Grammar.IntType _) = IntFun
extractFunReturnType (Grammar.StringType _) = StringFun
extractFunReturnType (Grammar.VoidType _) = VoidFun

addParamsToScope :: Scope -> Grammar.FunDec -> Scope
addParamsToScope s (Grammar.FunDec _ _ p _ _) = foldl addParamToScope newScope extractedParams where
    newScope = NestedScope emptyEnvironment s
    extractedParams = case p of
        (Grammar.Params (Grammar.ParamList ps _ ) _) -> ps
        (Grammar.EmptyParams _) -> []

addParamToScope :: Scope -> Grammar.Param -> Scope
addParamToScope s p = addToScope s (extractParamName p) (VarDecType $ silentlyExtractParamType p)

extractParamName :: Grammar.Param -> Identifier
extractParamName (Grammar.Param _ _ result _) = result

extractParamType :: Grammar.Param -> DecType
extractParamType (Grammar.Param t m i l) = result where
    var = (rawType, metaType)
    debugMessage = "Line " ++ show l ++ ": variable " ++ i ++ " assigned type " ++ showVar var
    result = debug debugMessage $ VarDecType var
    rawType = extractRawVarType t i l
    metaType = extractParamMetaType m

silentlyExtractParamType :: Grammar.Param -> VarType
silentlyExtractParamType (Grammar.Param t m i l) = (rawType, metaType) where
    rawType = extractRawVarType t i l
    metaType = extractParamMetaType m

extractParamMetaType :: Grammar.ParamMetaType -> VarMetaType
extractParamMetaType Grammar.RawParam = RawVar
extractParamMetaType Grammar.PointerParam = PointerVar
extractParamMetaType Grammar.ArrayParam = ArrayVar 0

processRelOp :: Grammar.RelOp -> RelOp
processRelOp (Grammar.LessThanOrEqualRelOp _) = LessThanOrEqualRelOp
processRelOp (Grammar.LessThanRelOp _) = LessThanRelOp
processRelOp (Grammar.EqualRelOp _) = EqualRelOp
processRelOp (Grammar.NotEqualRelOp _) = NotEqualRelOp
processRelOp (Grammar.GreaterThanRelOp _) = GreaterThanRelOp
processRelOp (Grammar.GreaterThanOrEqualRelOp _) = GreaterThanOrEqualRelOp

processAddOp :: Grammar.AddOp -> AddOp
processAddOp (Grammar.PlusAddOp _) = PlusAddOp
processAddOp (Grammar.MinusAddOp _) = MinusAddOp

processMulOp :: Grammar.MulOp -> MulOp
processMulOp (Grammar.TimesMulOp _) = TimesMulOp
processMulOp (Grammar.DivMulOp _) = DivMulOp
processMulOp (Grammar.ModMulOp _) = ModMulOp

extractLValueFromExpression :: Scope -> Grammar.Expression -> LValue
extractLValueFromExpression s
    (Grammar.SimpleExpression
    (Grammar.SimpleExp
    (Grammar.SimpleE
    (Grammar.SimpleT
    (Grammar.SimpleF f _) _) _) _) _) = processLValue s f
extractLValueFromExpression s (Grammar.AssignmentExpression _ e _) = result where
    result = extractLValueFromExpression s e
extractLValueFromExpression _ line = error $
    "Line " ++ show line ++ ": cannot reference non-lvalue"

extractLineFromFactor :: Grammar.Factor -> LineNumber
extractLineFromFactor (Grammar.GroupedFactor _ l) = l
extractLineFromFactor (Grammar.FunCallFactor _ l) = l
extractLineFromFactor (Grammar.ReadFactor l) = l
extractLineFromFactor (Grammar.DereferenceFactor _ l) = l
extractLineFromFactor (Grammar.VarFactor _ l) = l
extractLineFromFactor (Grammar.ArrayReferenceFactor _ _ l) = l
extractLineFromFactor (Grammar.NumberFactor _ l) = l
extractLineFromFactor (Grammar.StringFactor _ l) = l
