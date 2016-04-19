module Analyzer where

import qualified Grammar
import Types
import Debug.Trace (trace)

debugging :: Bool
debugging = True

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

processProgram :: Grammar.Program -> TypedProgram
processProgram (Grammar.Program (Grammar.DeclarationList ds _) _) = verifiedResult where
    (s, ts) = splitScan programHelper emptyScope ds
    mainDec = if Types.contains s "main"
        then Types.lookup s "main" 1
        else error "Programs must contain a main function"
    mainFun = case mainDec of
        VarDecType _ -> error "Declaration for main must correspond to a function"
        FunDecType f -> f
    verifiedResult = if funReturn mainFun == VoidFun
        then
            if null $ funArgs mainFun
                then TypedProgram ts
                else error "Main function cannot take any arguments"
        else
            error "Main function must have type void"

programHelper :: Scope -> Grammar.Declaration -> (Scope, TypedDeclaration)
programHelper s d = case d of
    (Grammar.VarDecDeclaration v _) -> (newScope, newDec) where
        newScope = addToScope s (extractVarName v) (VarDecType varType)
        newDec = processDeclaration newScope d
        varType = silentlyExtractVarType v
    (Grammar.FunDecDeclaration f _) -> (newScope, newDec) where
        newScope = addFunToScope s f
        newDec = processDeclaration newScope d

processDeclaration :: Scope -> Grammar.Declaration -> TypedDeclaration
processDeclaration _ (Grammar.VarDecDeclaration v _) = result where
    result = TypedVarDec (extractVarName v) (extractVarType v)
processDeclaration s (Grammar.FunDecDeclaration (Grammar.FunDec t i p c l) _) = result where
    result = TypedFunDec i returnType params body
    params = processParams p
    newScope = addParamsToScope s (Grammar.FunDec t i p c l)
    returnType = extractFunReturnType t
    body = processCompoundStmt newScope (funNodeType (returnType, [])) c

processParams :: Grammar.Params -> [(Identifier, VarType)]
processParams (Grammar.EmptyParams _) = []
processParams (Grammar.Params (Grammar.ParamList ps _) _) = zip is vs where
    is = map extractParamName ps
    vs = map silentlyExtractParamType ps

processCompoundStmt :: Scope -> NodeType -> Grammar.CompoundStmt -> TypedCompoundStmt
processCompoundStmt _ _ (Grammar.CompoundStmt _ (Grammar.EmptyStatementList _) _) =
    TypedCompoundStmt (TypedLocalDecs []) []
processCompoundStmt s n (Grammar.CompoundStmt l (Grammar.StatementList ss _) _) =
    TypedCompoundStmt localDecs statements where
        localDecs = processLocalDecs l
        decList = case localDecs of
            (TypedLocalDecs ds) -> ds
        statements = map (processStatement newScope n) ss
        newScope = foldl addVarToScope s decList
        addVarToScope s' (i, v) = addToScope s' i (VarDecType v)

processLocalDecs :: Grammar.LocalDecs -> TypedLocalDecs
processLocalDecs (Grammar.EmptyLocalDecs _) = TypedLocalDecs []
processLocalDecs (Grammar.LocalDecs vs _) = TypedLocalDecs (map processLocalVarDec vs)

processLocalVarDec :: Grammar.VarDec -> (Identifier, VarType)
processLocalVarDec v = (i, t) where
    i = extractVarName v
    t = extractVarType v

processStatement :: Scope -> NodeType -> Grammar.Statement -> TypedStatement
processStatement s _ (Grammar.ExpressionStmt e _) =
    TypedExpressionStmt (processExpression s e)
processStatement _ _ (Grammar.EmptyExpressionStmt _) =
    TypedEmptyExpressionStmt
processStatement s n (Grammar.CompoundStmtStmt c _) =
    TypedCompoundStmtStmt (processCompoundStmt s n c)
processStatement s n (Grammar.IfStmt e s' line) =
    verifiedResult where
        e' = processExpression s e
        s'' = processStatement s n s'
        verifiedResult = case nodeType e' of
            (IntNode, RawNode) -> TypedIfStmt e' s''
            _ -> error $
                "Line " ++ show line ++ ": clause of if-statement must be of type raw integer"
processStatement s n (Grammar.IfElseStmt e s' s'' line) =
    verifiedResult where
        e' = processExpression s e
        s''' = processStatement s n s'
        s'''' = processStatement s n s''
        verifiedResult = case nodeType e' of
            (IntNode, RawNode) -> TypedIfElseStmt e' s''' s''''
            _ -> error $
                "Line " ++ show line ++ ": clause of if-else-statement must be of type raw integer"
processStatement s n (Grammar.WhileStmt e s' line) =
    verifiedResult where
        e' = processExpression s e
        s'' = processStatement s n s'
        verifiedResult = case nodeType e' of
            (IntNode, RawNode) -> TypedWhileStmt e' s''
            _ -> error $
                "Line " ++ show line ++ ": clause of while-statement must be of type raw integer"
processStatement s n (Grammar.ReturnStmt e line) =
    verifiedResult where
        e' = processExpression s e
        verifiedResult = if nodeType e' == n
            then
                if nodeRaw n /= VoidNode
                    then TypedReturnStmt e'
                    else error $
                        "Line " ++ show line ++ ": void function cannot have non-empty return statement"
                else error $
                    "Line " ++ show line ++ ": return type does not match function type"
processStatement _ n (Grammar.EmptyReturnStmt line) =
    verifiedResult where
        verifiedResult = if nodeRaw n == VoidNode
            then TypedEmptyReturnStmt
            else error $
                "Line " ++ show line ++ ": non-void function cannot have empty return statement"
processStatement s _ (Grammar.WriteStmt e line) =
    verifiedResult where
        e' = processExpression s e
        verifiedResult = case nodeType e' of
            (IntNode, RawNode) -> TypedWriteStmt e'
            (StringNode, RawNode) -> TypedWriteStmt e'
            _ -> error $
                "Line " ++ show line ++ ": arguments to write statement can only be of type raw integer or raw string"
processStatement _ _ (Grammar.WritelnStmt _) =
    TypedWritelnStmt

processExpression :: Scope -> Grammar.Expression -> TypedExpression
processExpression s (Grammar.AssignmentExpression (Grammar.Var i m l) e line) =
    TypedAssignmentExpression i a e' loggedNode where
        dec = Types.lookup s i l
        var = case dec of
            VarDecType v -> v
            FunDecType _ -> error $
                "Line " ++ show line ++ ": cannot assign to function "  ++ i
        alteredVar = case m of
            Grammar.RawVar -> var
            Grammar.PointerVar -> case varMeta var of
                PointerVar -> (varRaw var, RawVar)
                RawVar -> error $
                    "Line " ++ show line ++ ": cannot assign to address of raw variable " ++ i
                ArrayVar -> error $
                    "Line " ++ show line ++ ": cannot assign to address of array variable " ++ i
            Grammar.ArrayVar _ -> case varMeta var of
                ArrayVar -> (varRaw var, RawVar)
                RawVar -> error $
                    "Line " ++ show line ++ ": cannot assign to index of raw variable " ++ i
                PointerVar -> error $
                    "Line " ++ show line ++ ": cannot assign to index of pointer variable " ++ i
        e' = processExpression s e
        a = case m of
            Grammar.RawVar -> RawAssignment
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
    TypedSimpleExpression compExp loggedNode where
        compExp = processCompExp s c
        node = nodeType compExp
        loggedNode = logAssignment node "Expression" line

processCompExp :: Scope -> Grammar.CompExp -> TypedCompExp
processCompExp s (Grammar.CompExp e r e' line) =
    TypedCompExp e'' r' e''' loggedNode where
        e'' = processE s e
        r' = toTypedRelOp r
        e''' = processE s e'
        rawInt = (IntNode, RawNode)
        verifiedNode = if nodeType e'' == rawInt && nodeType e''' == rawInt
            then rawInt
            else error $
                "Line " ++ show line ++ ": both operands of CompExp must be of type raw integer"
        loggedNode = logAssignment verifiedNode "CompExp" line
processCompExp s (Grammar.SimpleExp e line) =
    TypedSimpleExp e' loggedNode where
        e' = processE s e
        node = nodeType e'
        loggedNode = logAssignment node "CompExp" line

processE :: Scope -> Grammar.E -> TypedE
processE s (Grammar.AddE e a t line) =
    TypedAddE e' a' t' loggedNode where
        e' = processE s e
        a' = toTypedAddOp a
        t' = processT s t
        rawInt = (IntNode, RawNode)
        verifiedNode = if nodeType e' == rawInt && nodeType t' == rawInt
            then rawInt
            else error $
                "Line " ++ show line ++ ": both operands of AddE must be of type raw integer"
        loggedNode = logAssignment verifiedNode "E" line
processE s (Grammar.SimpleE t line) =
    TypedSimpleE t' loggedNode where
        t' = processT s t
        node = nodeType t'
        loggedNode = logAssignment node "E" line

processT :: Scope -> Grammar.T -> TypedT
processT s (Grammar.MulT t m f line) =
    TypedMulT t' m' f' loggedNode where
        t' = processT s t
        m' = toTypedMulOp m
        f' = processF s f
        rawInt = (IntNode, RawNode)
        verifiedNode = if nodeType t' == rawInt && nodeType f' == rawInt
            then rawInt
            else error $
                "Line " ++ show line ++ ": both operands of MulT must be of type raw integer"
        loggedNode = logAssignment verifiedNode "T" line
processT s (Grammar.SimpleT f line) =
    TypedSimpleT f' loggedNode where
        f' = processF s f
        node = nodeType f'
        loggedNode = logAssignment node "T" line

processF :: Scope -> Grammar.F -> TypedF
processF s (Grammar.NegativeF f line) =
    result where
        f' = processF s f
        node = nodeType f'
        verifiedNode = case node of
            (IntNode, RawNode) -> node
            _ -> error $
                "Line " ++ show line ++ ": negative F must have child of type raw integer"
        loggedNode = logAssignment verifiedNode "F" line
        result = TypedNegativeF f' loggedNode
processF s (Grammar.ReferenceF factor line) =
    result where
        factor' = processFactor s factor
        node = nodeType factor'
        verifiedNode = case node of
            (r, RawNode) -> (r, PointerNode)
            (_, PointerNode) -> error $
                "Line " ++ show line ++ ": cannot reference pointer factor"
            (_, ArrayNode) -> error $
                "Line " ++ show line ++ ": cannot reference array factor"
        loggedNode = logAssignment verifiedNode "F" line
        result = TypedReferenceF factor' loggedNode
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
        result = TypedDereferenceF factor' loggedNode
processF s (Grammar.SimpleF factor line) =
    TypedSimpleF factor' loggedNode where
        factor' = processFactor s factor
        loggedNode = logAssignment (nodeType factor') "F" line

processFactor :: Scope -> Grammar.Factor -> TypedFactor
processFactor s (Grammar.GroupedFactor e line) =
    TypedGroupedFactor expression loggedNode where
        expression = processExpression s e
        loggedNode = logAssignment (nodeType expression) "Factor" line
processFactor s (Grammar.FunCallFactor f line) =
    TypedFunCallFactor funCall loggedNode where
        funCall = processFunCall s f
        loggedNode = logAssignment (nodeType funCall) "Factor" line
processFactor _ (Grammar.ReadFactor line) = result where
    loggedNode = logAssignment (IntNode, RawNode) "Factor" line
    result = TypedReadFactor loggedNode
processFactor s (Grammar.DereferenceFactor i line) =
    TypedVarFactor i loggedNode where
        dec = Types.lookup s i line
        var = case dec of
            VarDecType v -> v
            FunDecType _ -> error $
                "Line " ++ show line ++ ": cannot dereference function " ++ i
        ptr = case var of
            (r, PointerVar) -> r
            (_, RawVar) -> error $
                "Line " ++ show line ++ ": cannot dereference raw variable " ++ i
            (_, ArrayVar) -> error $
                "Line " ++ show line ++ ": cannot dereference array variable " ++ i
        loggedNode = logAssignment (varNodeType (ptr, PointerVar)) "Factor" line
processFactor s (Grammar.VarFactor i line) =
    TypedVarFactor i loggedNode where
        dec = Types.lookup s i line
        var = case dec of
            VarDecType v -> v
            FunDecType _ -> error $
                "Line " ++ show line ++ ": cannot reference function " ++ i ++ " without calling it"
        loggedNode = logAssignment (varNodeType var) "Factor" line
processFactor s (Grammar.ArrayReferenceFactor i e line) =
    TypedArrayReferenceFactor i verifiedIndex loggedNode where
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
            (a, ArrayVar) -> a
            (_, RawVar) -> error $
                "Line " ++ show line ++ ": cannot index raw variable " ++ i
            (_, PointerVar) -> error $
                "Line " ++ show line ++ ": cannot index pointer variable " ++ i
        loggedNode = logAssignment (varNodeType (arr, RawVar)) "Factor" line
processFactor _ (Grammar.NumberFactor n line) = result where
    loggedNode = logAssignment (IntNode, RawNode) "Factor" line
    result = TypedNumberFactor n loggedNode
processFactor _ (Grammar.StringFactor s' line) = result where
    loggedNode = logAssignment (StringNode, RawNode) "Factor" line
    result = TypedStringFactor s' loggedNode

processFunCall :: Scope -> Grammar.FunCall -> TypedFunCall
processFunCall s (Grammar.FunCall i (Grammar.EmptyArgs _) line) =
    TypedFunCall i [] loggedReturnType where
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
    TypedFunCall i args loggedReturnType where
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
extractMetaVarType (Grammar.ArrayVarDec _) = ArrayVar

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
extractParamMetaType Grammar.ArrayParam = ArrayVar

toTypedRelOp :: Grammar.RelOp -> TypedRelOp
toTypedRelOp (Grammar.LessThanOrEqualRelOp _) = TypedLessThanOrEqualRelOp
toTypedRelOp (Grammar.LessThanRelOp _) = TypedLessThanRelOp
toTypedRelOp (Grammar.EqualRelOp _) = TypedEqualRelOp
toTypedRelOp (Grammar.NotEqualRelOp _) = TypedNotEqualRelOp
toTypedRelOp (Grammar.GreaterThanRelOp _) = TypedGreaterThanRelOp
toTypedRelOp (Grammar.GreaterThanOrEqualRelOp _) = TypedGreaterThanOrEqualRelOp

toTypedAddOp :: Grammar.AddOp -> TypedAddOp
toTypedAddOp (Grammar.PlusAddOp _) = TypedPlusAddOp
toTypedAddOp (Grammar.MinusAddOp _) = TypedMinusAddOp

toTypedMulOp :: Grammar.MulOp -> TypedMulOp
toTypedMulOp (Grammar.TimesMulOp _) = TypedTimesMulOp
toTypedMulOp (Grammar.DivMulOp _) = TypedDivMulOp
toTypedMulOp (Grammar.ModMulOp _) = TypedModMulOp
