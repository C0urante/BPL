module Analyzer where

import Grammar
import qualified Environment as Env
import Debug.Trace (trace)
import Data.Maybe

analyze :: Program -> Bool
analyze = typeCheck . assignTypes

type ScopedStatementList = ([Statement], Env.Scope, Env.FunType)

debugging :: Bool
debugging = True

debug :: String -> b -> b
debug info result
    | debugging = trace info result
    | otherwise = result


-- Takes in a Program, and returns a series of lists of Statements paired with
-- their accompanying scopes. Throws an error if global declarations conflict,
-- and ignores all empty statement lists.
assignTypes :: Program -> [ScopedStatementList]
assignTypes (Program (DeclarationList ds _) _) =
    declarationHelper ds (Env.GlobalScope Env.emptyEnvironment)

declarationHelper :: [Declaration] -> Env.Scope -> [ScopedStatementList]
declarationHelper [] s = verifiedResult where
    hasMain = Env.contains s "main"
    mainType =
        if hasMain
            then Env.lookup s "main"
            else error "Program must contain main function"
    mainFun =
        case mainType of
            (Env.Function f) -> f
            (Env.Variable _) -> error "Identifier main must correspond to function at global scope"
    mainHasVoidReturn  =
        case mainFun of
            (Env.VoidFun, _) -> True
            _ -> error "Function main must have return type void"
    mainHasVoidArgs =
        case mainFun of
            (_, []) -> True
            _ -> error "Function main cannot take any arguments"
    verifiedResult = seq (mainHasVoidReturn && mainHasVoidArgs) []
declarationHelper (VarDecDeclaration v _:ds) s =
    declarationHelper ds (addVarToScope s v)
declarationHelper (FunDecDeclaration f _:ds) s =
    functionStatements ++ declarationHelper ds newGlobalScope where
        newGlobalScope = addFunToScope s f
        newFunctionScope = addParamsToScope newGlobalScope f
        funReturnType = extractFunReturnType f
        functionStatements = compoundStmtHelper newFunctionScope funReturnType (extractFunBody f)

-- Takes in a Statement, and returns whether or not it has an internal Statement
-- that does not require a new Scope
hasScopelessStatement :: Statement -> Bool
hasScopelessStatement (CompoundStmtStmt _ _) = False
hasScopelessStatement _ = True

-- Takes in a Statement, and if that Statement contains a CompoundStmt, returns it
extractCompoundStmt :: Statement -> Maybe CompoundStmt
extractCompoundStmt s = case s of
    (ExpressionStmt _ _) -> Nothing
    (EmptyExpressionStmt _) -> Nothing
    (CompoundStmtStmt result _) -> Just result
    (IfStmt _ s' _) -> extractCompoundStmt s'
    (IfElseStmt _ s' _ _) -> extractCompoundStmt s'
    (WhileStmt _ s' _) -> extractCompoundStmt s'
    (ReturnStmt _ _) -> Nothing
    (EmptyReturnStmt _) -> Nothing
    (WriteStmt _ _) -> Nothing
    (WritelnStmt _) -> Nothing

compoundStmtHelper :: Env.Scope -> Env.FunType -> CompoundStmt -> [ScopedStatementList]
compoundStmtHelper _ _ (CompoundStmt _ (EmptyStatementList l) _) =
    debug ("Disregarding empty statement list at line " ++ show l) []
compoundStmtHelper s r (CompoundStmt (LocalDecs vs _) (StatementList ss _) _) =
    localScopedStatementList:nestedStatementsList where
        emptyLocalScope = Env.NestedScope Env.emptyEnvironment s
        populatedLocalScope = foldl addVarToScope emptyLocalScope vs
        localStatements = filter hasScopelessStatement ss
        localScopedStatementList = (localStatements, populatedLocalScope, r)
        nestedStatements = map fromJust $ filter isJust $ map extractCompoundStmt ss
        nestedStatementsList = concatMap (compoundStmtHelper populatedLocalScope r) nestedStatements
compoundStmtHelper s r (CompoundStmt (EmptyLocalDecs _) (StatementList ss _) _) =
    localScopedStatementList:nestedStatementsList where
        localScope = s
        localStatements = filter hasScopelessStatement ss
        localScopedStatementList = (localStatements, localScope, r)
        nestedStatements = map fromJust $ filter isJust $ map extractCompoundStmt ss
        nestedStatementsList = concatMap (compoundStmtHelper localScope r) nestedStatements

addVarToScope :: Env.Scope -> VarDec -> Env.Scope
addVarToScope s v = Env.addToScope s i t where
    i = extractVarName v
    t = extractVarType v

extractVarName :: VarDec -> Identifier
extractVarName (VarDec _ _ result _) = result

extractVarLine :: VarDec -> LineNumber
extractVarLine (VarDec _ _ _ result) = result

extractVarType :: VarDec -> Env.Type
extractVarType (VarDec t m i l) = result where
    var = (rawType t, metaType m)
    debugMessage = "Line " ++ show l ++ ": variable " ++ i ++ " assigned type " ++ Env.showVar var
    result = debug debugMessage $ Env.Variable var
    rawType (IntType _) = Env.IntVar
    rawType (StringType _) = Env.StringVar
    rawType (VoidType _) =
        error $ "Cannot assign void type to variable " ++ i ++ " declared at line " ++ show l
    metaType RawVarDec = Env.RawVar
    metaType PointerVarDec = Env.PointerVar
    metaType (ArrayVarDec _) = Env.ArrayVar

addFunToScope :: Env.Scope -> FunDec -> Env.Scope
addFunToScope s f = Env.addToScope s i t where
    i = extractFunName f
    t = extractFunType f

extractFunName :: FunDec -> Identifier
extractFunName (FunDec _ result _ _ _) = result

extractFunBody :: FunDec -> CompoundStmt
extractFunBody (FunDec _ _ _ result _) = result

extractFunType :: FunDec -> Env.Type
extractFunType (FunDec t i p _ l) = Env.Function (returnType t, paramTypes p) where
    returnType (IntType _) =
        debug ("Line " ++ show l ++ ": function " ++ i ++ " assigned return type integer") Env.IntFun
    returnType (StringType _) =
        debug ("Line " ++ show l ++ ": function " ++ i ++ " assigned return type string") Env.StringFun
    returnType (VoidType _) =
        debug ("Line " ++ show l ++ ": function " ++ i ++ " assigned return type void") Env.VoidFun
    paramTypes (EmptyParams _) = []
    paramTypes (Params (ParamList p' _) _) = map (extractVar . extractParamType) p'
    extractVar (Env.Variable v) = v

extractFunReturnType :: FunDec -> Env.FunType
extractFunReturnType (FunDec t _ _ _ _) = returnType t where
    returnType (IntType _) = Env.IntFun
    returnType (StringType _) = Env.StringFun
    returnType (VoidType _) = Env.VoidFun

addParamsToScope :: Env.Scope -> FunDec -> Env.Scope
addParamsToScope s (FunDec _ _ p _ _) = foldl addParamToScope newScope extractedParams where
    newScope = Env.NestedScope Env.emptyEnvironment s
    extractedParams = case p of
        (Params (ParamList ps _ ) _) -> ps
        (EmptyParams _) -> []

addParamToScope :: Env.Scope -> Param -> Env.Scope
addParamToScope s p = Env.addToScope s (extractParamName p) (silentlyExtractParamType p)

extractParamName :: Param -> Identifier
extractParamName (Param _ _ result _) = result

extractParamType :: Param -> Env.Type
extractParamType (Param t m i l) = result where
    var = (rawType t, metaType m)
    debugMessage = "Line " ++ show l ++ ": variable " ++ i ++ " assigned type " ++ Env.showVar var
    result = debug debugMessage $ Env.Variable var
    rawType (IntType _) = Env.IntVar
    rawType (StringType _) = Env.StringVar
    rawType (VoidType _) =
        error $ "Cannot assign void type to variable " ++ i ++ " declared at line " ++ show l
    metaType RawParam = Env.RawVar
    metaType PointerParam = Env.PointerVar
    metaType ArrayParam = Env.ArrayVar

silentlyExtractParamType :: Param -> Env.Type
silentlyExtractParamType (Param t m i l) = Env.Variable (rawType t, metaType m) where
    rawType (IntType _) = Env.IntVar
    rawType (StringType _) = Env.StringVar
    rawType (VoidType _) =
        error $ "Cannot assign void type to variable " ++ i ++ " declared at line " ++ show l
    metaType RawParam = Env.RawVar
    metaType PointerParam = Env.PointerVar
    metaType ArrayParam = Env.ArrayVar


-- END TYPE ASSIGNMENT -- END TYPE ASSGINMENT -- END TYPE ASSIGNMENT -- END TYPE ASSIGNMENT --
----------------------------------------------------------------------------------------------
-- BEGIN TYPE CHECKING -- BEGIN TYPE CHECKING -- BEGIN TYPE CHECKING -- BEGIN TYPE CHECKING --

logAssignment :: Env.Var -> String -> LineNumber -> Env.Var
logAssignment v c l = debug message v where
    message = "Line " ++ show l ++ ": " ++ c ++ " assigned type " ++ Env.showVar v

typeCheck :: [ScopedStatementList] -> Bool
typeCheck = all statementListCheck

statementListCheck :: ScopedStatementList -> Bool
statementListCheck (ss, s, r) = all (statementCheck s r) ss

statementCheck :: Env.Scope -> Env.FunType -> Statement -> Bool
statementCheck s _ (ExpressionStmt e _) =
    seq (expressionType s e) True
statementCheck _ _ (EmptyExpressionStmt _) =
    True
statementCheck s r (IfStmt e s' _) =
    isBooleanType (expressionType s e) && statementCheck s r s'
statementCheck s r (IfElseStmt e s' s'' _) =
    isBooleanType (expressionType s e) && statementCheck s r s' && statementCheck s r s''
statementCheck s r (WhileStmt e s' _) =
    isBooleanType (expressionType s e) && statementCheck s r s'
statementCheck s r (ReturnStmt e _) =
    expressionType s e == Env.funTypeToVar r
statementCheck _ r (EmptyReturnStmt _) =
    isVoidType r
statementCheck s _ (WriteStmt e _) =
    isWriteType (expressionType s e)
statementCheck _ _ (WritelnStmt _) = True
-- All CompoundStmtStmts will have been added to another ScopedStatementList; no
-- need to check them here, but we do need to account for the case to make
-- recursion with the bodies of if/while statements easier
statementCheck _ _ (CompoundStmtStmt _ _) = True

expressionType :: Env.Scope -> Expression -> Env.Var
expressionType s (SimpleExpression c l) = logAssignment result "Expression" l where
    result = compExpType s c
expressionType s (AssignmentExpression v e l) = logAssignment verifiedResult "Expression" l where
    verifiedResult =
        if leftHandType == rightHandType
            then leftHandType
            else error $ "Line " ++ show l ++ ": left hand and right hand types do not match"
    rightHandType = expressionType s e
    leftHandType = leftHandTransformedType (leftHandScopeType v) v
    leftHandScopeType (Var i _ _) = case Env.lookup s i of
        (Env.Function _) -> error $ "Line " ++ show l ++ ": cannot assign to function"
        (Env.Variable (t, m)) -> (t, m)
    leftHandTransformedType :: Env.Var -> Var -> Env.Var
    leftHandTransformedType (t, m) (Var _ a _) = case (t, m, a) of
        (t', m', RawVar) -> (t', m')
        (t', Env.PointerVar, PointerVar) -> (t', Env.RawVar)
        (_, Env.PointerVar, ArrayVar _) ->
            error $ "Line " ++ show l ++ ": cannot access pointer via index"
        (t', Env.ArrayVar, ArrayVar e') ->
            if isIntegerType $ expressionType s e'
                then (t', Env.RawVar)
                else error $ "Line " ++ show l ++ ": array indices must be integer type"
        (_, Env.ArrayVar, PointerVar) ->
            error $ "Line " ++ show l ++ ": cannot dereference array"

compExpType :: Env.Scope -> CompExp -> Env.Var
compExpType s (SimpleExp e l) = logAssignment result "CompExp" l where
    result = eType s e
compExpType s (CompExp e _ e' l) = logAssignment verifiedResult "CompExp" l where
    verifiedResult =
        if isIntegerType leftHandType && isIntegerType rightHandType
            then (Env.IntVar, Env.RawVar)
            else error $ "Line " ++ show l ++ ": both sides of CompExp must have type raw integer"
    leftHandType = eType s e
    rightHandType = eType s e'

eType :: Env.Scope -> E -> Env.Var
eType s (SimpleE t l) = logAssignment result "E" l where
    result = tType s t
eType s (AddE e _ t l) = logAssignment verifiedResult "E" l where
    verifiedResult =
        if isIntegerType leftHandType && isIntegerType rightHandType
            then (Env.IntVar, Env.RawVar)
            else error $ "Line " ++ show l ++ ": both sides of AddE must have type raw integer"
    leftHandType = eType s e
    rightHandType = tType s t

tType :: Env.Scope -> T -> Env.Var
tType s (SimpleT f l) = logAssignment result "T" l where
    result = fType s f
tType s (MulT t _ f l) = logAssignment verifiedResult "T" l where
    verifiedResult =
        if isIntegerType leftHandType && isIntegerType rightHandType
            then (Env.IntVar, Env.RawVar)
            else error $ "Line " ++ show l ++ ": both sides of MulT must have type raw integer"
    leftHandType = tType s t
    rightHandType = fType s f

fType :: Env.Scope -> F -> Env.Var
fType s (NegativeF f l) = logAssignment verifiedResult "F" l where
    verifiedResult =
        if isIntegerType operandType
            then (Env.IntVar, Env.RawVar)
            else error $ "Line " ++ show l ++ ": unary negative operator requires raw integer type"
    operandType = fType s f
fType s (ReferenceF factor l) = logAssignment verifiedResult "F" l where
    verifiedResult = case operandType of
        (t, Env.RawVar) -> (t, Env.PointerVar)
        (_, Env.ArrayVar) -> error $ "Line " ++ show l ++ ": cannot reference array type"
        (_, Env.PointerVar) -> error $ "Line " ++ show l ++ ": cannot reference pointer type"
    operandType = factorType s factor
fType s (DereferenceF factor l) = logAssignment verifiedResult "F" l where
    verifiedResult = case operandType of
        (t, Env.PointerVar) -> (t, Env.RawVar)
        (_, Env.RawVar) -> error $ "Line " ++ show l ++ ": cannot dereference raw type"
        (_, Env.ArrayVar) -> error $ "Line " ++ show l ++ ": cannot dereference array type"
    operandType = factorType s factor
fType s (SimpleF factor l) = logAssignment result "F" l where
    result = factorType s factor

factorType :: Env.Scope -> Factor -> Env.Var
factorType s (GroupedFactor e l) = logAssignment result "Factor" l where
    result = expressionType s e
factorType s (FunCallFactor fc l) = logAssignment result "Factor" l where
    result = funCallType s fc
factorType _ (ReadFactor l) = logAssignment result "Factor" l where
    result = (Env.StringVar, Env.RawVar)
factorType s (DereferenceFactor i l) = logAssignment verifiedResult "Factor" l where
    scopeVarType = Env.lookup s i
    verifiedResult = case scopeVarType of
        (Env.Variable (t, Env.PointerVar)) -> (t, Env.PointerVar)
        (Env.Variable (_, Env.RawVar)) -> error $ "Line " ++ show l ++ ": cannot dereference raw type"
        (Env.Variable (_, Env.ArrayVar)) -> error $ "Line " ++ show l ++ ": cannot dereference array type"
        (Env.Function _) -> error $ "Line " ++ show l ++ ": function illegally used outside context of either declaration or call"
factorType s (VarFactor i l) = logAssignment verifiedResult "Factor" l where
    verifiedResult = case scopedVarType of
        (Env.Variable v) -> v
        (Env.Function _) -> error $ "Line " ++ show l ++ ": function illegally used outside context of either declaration or call"
    scopedVarType = Env.lookup s i
factorType s (ArrayReferenceFactor i e l) = logAssignment verifiedResult "Factor" l where
    verifiedResult = case scopedVarType of
        (Env.Variable (t, Env.ArrayVar)) ->
            if isIntegerType $ expressionType s e
                then (t, Env.RawVar)
                else error $ "Line " ++ show l ++ ": array indices must be integer type"
        (Env.Variable (_, Env.RawVar)) -> error $ "Line " ++ show l ++ ": cannot index raw type"
        (Env.Variable (_, Env.PointerVar)) -> error $ "Line " ++ show l ++ ": cannot index pointer type"
        (Env.Function _) -> error $ "Line " ++ show l ++ ": function illegally used outside context of either declaration or call"
    scopedVarType = Env.lookup s i
factorType _ (NumberFactor _ l) = logAssignment result "Factor" l where
    result = (Env.IntVar, Env.RawVar)
factorType _ (StringFactor _ l) = logAssignment result "Factor" l where
    result = (Env.StringVar, Env.RawVar)

funCallType :: Env.Scope -> FunCall -> Env.Var
funCallType s (FunCall i a l) = logAssignment verifiedResult "FunCall" l where
    verifiedResult =
        if scopedFunArgs == extractArgTypes a
            then Env.funTypeToVar scopedFunType
            else error $ "Line " ++ show l ++ ": incorrect argument types supplied to function"
    (scopedFunType, scopedFunArgs) = extractFun $ Env.lookup s i
    extractFun (Env.Variable _) = error $ "Line " ++ show l ++ ": cannot call non-function"
    extractFun (Env.Function f) = f
    extractArgTypes (EmptyArgs _) = []
    extractArgTypes (Args (ArgList as _) _) = map (expressionType s) as

isIntegerType :: Env.Var -> Bool
isIntegerType = (==) (Env.IntVar, Env.RawVar)

isBooleanType :: Env.Var -> Bool
isBooleanType = isIntegerType

isVoidType :: Env.FunType -> Bool
isVoidType Env.VoidFun = True
isVoidType _ = False

isWriteType :: Env.Var -> Bool
isWriteType (t, m) = m == Env.RawVar && (t == Env.IntVar || t == Env.StringVar)
