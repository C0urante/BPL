module Compiler where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (partition)
import Types hiding (Scope)
import Instruction

type Variable = (Identifier, VarType)
type Function = (Identifier, FunReturnType, [(Identifier, VarType)], CompoundStmt)
type FrameSize = Int

--  Integers correspond to frame offsets for locals, and Labels for the actual
-- assembly label used for globals and functions; array parameters have to be
-- treated somewhat specially, since they are passed as pointers
data ScopeValue =
    LocalVariable Int |
    ArrayParameter Int |
    GlobalVariable Label |
    Function Label
type Scope = Map.Map Identifier ScopeValue
type Strings = Map.Map String Label
type Code = [AssemblyLine]

compile :: Program -> Code
compile p = result where
    (readOnlyCode, strings) = readOnlySection p
    (dataCode, scope) = dataSection p
    textCode = textSection p scope strings
    result = readOnlyCode ++ dataCode ++ textCode

labelStream :: String -> [Label]
labelStream prefix = map (((".L" ++ prefix) ++) . show) ([0..] :: [Int])

--  The only purpose for this is to generate directives for string literals used in the
-- program and in calls to printf/scanf, and track the labels given to those string literals.
readOnlySection :: Program -> (Code, Strings)
readOnlySection p = (code, strings) where
    stringLiterals = extractStringLiterals p
    (stringCode, strings) = stringLabels stringLiterals
    code = [ReadOnlyDirective] ++ readLabelCode ++ writeLabelCode ++ stringCode

readLabelCode :: Code
readLabelCode = [StringDirective ".integerRead" "%d"]

writeLabelCode :: Code
writeLabelCode = [newLineWrite, stringWrite, integerWrite] where
    newLineWrite = StringDirective ".newLineWrite" "\n"
    stringWrite =  StringDirective ".stringWrite"  "%s "
    integerWrite = StringDirective ".integerWrite" "%d "

-- Have to generate code that assigns each string a label, but also have to keep
-- track of each of these labels and return the remainder of the label stream.
stringLabels :: [String] -> (Code, Strings)
stringLabels ss = (code, strings) where
    stringLine (s, l) = StringDirective l s
    addStringToMap m (s, l) = Map.insert s l m
    initMap = Map.empty
    stringsAndLabels = zip ss labels
    code = map stringLine stringsAndLabels
    strings = foldl addStringToMap initMap stringsAndLabels
    labels = labelStream "S"

-- Find all string literals contained in a Program
extractStringLiterals :: Program -> [String]
extractStringLiterals p = concatMap extractFromFunction $ extractFunctions p where
    extractFromFunction (_, _, _, c) = setNub $ extractFromCompoundStmt c
    extractFromCompoundStmt (CompoundStmt _ ss) = concatMap extractFromStatement ss
    extractFromStatement (ExpressionStmt e) = extractFromExpression e
    extractFromStatement (CompoundStmtStmt c) = extractFromCompoundStmt c
    extractFromStatement (IfStmt e s) = extractFromExpression e ++ extractFromStatement s
    extractFromStatement (IfElseStmt e s s') = extractFromExpression e ++ extractFromStatement s ++ extractFromStatement s'
    extractFromStatement (WhileStmt e s) = extractFromExpression e ++ extractFromStatement s
    extractFromStatement (ReturnStmt e) = extractFromExpression e
    extractFromStatement (WriteStmt e) = extractFromExpression e
    extractFromStatement _ = []
    extractFromExpression (AssignmentExpression _ _ e _) = extractFromExpression e
    extractFromExpression (SimpleExpression c _) = extractFromCompExp c
    extractFromCompExp (SimpleExp e _) = extractFromE e
    extractFromCompExp _ = []
    extractFromE (SimpleE t _) = extractFromT t
    extractFromE _ = []
    extractFromT (SimpleT f _) = extractFromF f
    extractFromT _ = []
    extractFromF (SimpleF f _) = extractFromFactor f
    extractFromF _ = []
    extractFromFactor (GroupedFactor e _) = extractFromExpression e
    extractFromFactor (StringFactor s _) = [s]
    extractFromFactor (FunCallFactor f _) = extractFromFunCall f
    extractFromFactor _ = []
    extractFromFunCall (FunCall _ es _) = concatMap extractFromExpression es

-- Generate assembly code to give each global variable an address in the data
-- section with the appropriate amount of data allocated, and keep track of these
-- addresses and the identifiers they correspond to.
dataSection :: Program -> (Code, Scope)
dataSection p = result where
    globals = extractGlobalVariables p
    isArray (_, (_, ArrayVar _)) = True
    isArray _  = False
    (arrays, vars) = partition isArray globals
    (arrayCode, arrayScope) = arrayDataSection arrays Map.empty
    (varCode, fullScope) = varDataSection vars arrayScope
    result = if null globals
        then ([], Map.empty)
        else ([DataDirective] ++ arrayCode ++ varCode, fullScope)

extractGlobalVariables :: Program -> [Variable]
extractGlobalVariables (Program ds) = map extractVariable $ filter isVarDec ds where
    isVarDec (VarDec _ _) = True
    isVarDec (FunDec _ _ _ _) = False
    extractVariable (VarDec i t) = (i, t)

arrayDataSection :: [Variable] -> Scope -> (Code, Scope)
arrayDataSection vs s = (code, scope) where
    arrayLine ((_, v), l) = CommDirective l (sizeOfVar v) 8
    addArrayToScope s' ((i, _), l) = Map.insert i (GlobalVariable l) s'
    arraysAndLabels = zip vs labels
    code = map arrayLine arraysAndLabels
    scope = foldl addArrayToScope s arraysAndLabels
    labels = labelStream "A"

varDataSection :: [Variable] -> Scope -> (Code, Scope)
varDataSection vs s = (code, scope) where
    varLine ((_, v), l) = CommDirective l (sizeOfVar v) 8
    addVarToScope s' ((i, _), l) = Map.insert i (GlobalVariable l) s'
    varsAndLabels = zip vs labels
    code = map varLine varsAndLabels
    scope = foldl addVarToScope s varsAndLabels
    labels = labelStream "V"

textSection :: Program -> Scope -> Strings -> Code
textSection p scope strings = result where
    result = [TextDirective, GlobalDirective "main"] ++ functionsCode
    newScope = foldl addFunctionToScope scope functionNames
    functions = extractFunctions p
    extractFunctionName (i, _, _, _) = i
    functionNames = map extractFunctionName functions
    addFunctionToScope s i = Map.insert i (Function i) s
    functionsCode = concatMap (processFunction newScope strings) functions

extractFunctions :: Program -> [Function]
extractFunctions (Program ds) = map extractFunction $ filter isFunDec ds where
    isFunDec (FunDec _ _ _ _) = True
    isFunDec (VarDec _ _) = False
    extractFunction (FunDec i r p c) = (i, r, p, c)

--------------------------------------------------------------------------------

--  Before the call the caller pushes the arguments onto the stack in reverse order (last argument first, first
-- argument last) then pushes the fp onto the stack and makes the call.
--  At the start of the call the callee puts the current stack pointer, which points at the return address, into
-- the frame pointer.
--  At the return the callee puts into eax or rax any return value, then pops off the stack any temporary data
-- saved there. The callee then executes a ret.
--  The return instruction pops the return address off the stack.
--  At the return the caller pops the stack into the fp, restoring fp into what it was before the call. The
-- caller then increments the stack by enough to pop the arguments that were pushed there. Any return value
-- will be found in eax or rax.
processFunction :: Scope -> Strings -> Function -> Code
processFunction scope strings (i, _, ps, c) = result where
    result = functionLabel ++ setFramePointerCode ++ bodyCode
    functionLabel = [LabelDirective i]
    labels = labelStream $ i ++ "C"
    (newScope, _) = foldl addArgToScope (scope, 8) ps -- Start at 8 to avoid touching 0(%rbx), where the return address is stored
    addArgToScope (s, f) (i', v) = case varMeta v of
        (ArrayVar _) -> (Map.insert i' (ArrayParameter f) s, f + 8)
        _ -> case sizeOfNode $ varNodeType v of
            4 -> (Map.insert i' (LocalVariable f) s, f + 4)
            8 -> (Map.insert i' (LocalVariable f) s, f + 8)
            size -> error $
                "Cannot accept parameter of size " ++ show size ++ " for function " ++ i ++ "."
    setFramePointerCode = [MoveInstruction (SourceRegister StackPointer) (DestinationRegister SaveOne)]
    (bodyCode, _) = processCompoundStmt c newScope strings 0 labels (i == "main")

processCompoundStmt :: CompoundStmt -> Scope -> Strings -> FrameSize -> [Label] -> Bool -> (Code, [Label])
processCompoundStmt (CompoundStmt ld ss) scope strings frameSize labels isMain = (code, remainingLabels) where
    code = stackDecrementCode ++ statementsCode ++ stackIncrementCode
    (newScope, newFrameSize, stackDecrementCode, stackIncrementCode) = processLocalDecs ld scope frameSize
    statementHelper (cs, ls) s = addStatementCode cs $ processStatement s newScope strings newFrameSize ls isMain
    addStatementCode cs (sc, ls) = (cs ++ sc, ls)
    (statementsCode, remainingLabels) = foldl statementHelper ([], labels) ss

--  Return the new scope and frame space after making room for each new variable, as well as
-- code for incrementing/decrementing the stack before entering/after exiting the block
processLocalDecs :: LocalDecs -> Scope -> FrameSize -> (Scope, FrameSize, Code, Code)
processLocalDecs (LocalDecs []) scope _ = (scope, 0, [], []) -- If there are no local decs, don't bother.
processLocalDecs (LocalDecs ivs) scope frameSize = result where
    result = (newScope, newFrameSize, decrementCode, incrementCode)
    isArray (_, (_, ArrayVar _)) = True
    isArray _  = False
    (arrays, vars) = partition isArray ivs
    (arrayScope, arraySpace) = processArrayDecs arrays scope frameSize
    (newScope, varSpace) = processVarDecs vars arrayScope (frameSize + arraySpace)
    newFrameSpace = arraySpace + varSpace
    commentOne = Comment $ "Making room for " ++ show (length vars) ++ " local variable(s) and " ++ show (length arrays) ++ " local array(s)."
    decrementCode = [commentOne, SubInstruction (SourceImmediate newFrameSpace) (DestinationRegister StackPointer)]
    incrementCode = [commentTwo, AddInstruction (SourceImmediate newFrameSpace) (DestinationRegister StackPointer)]
    commentTwo = Comment $ "Reclaiming room originally allocated for " ++ show (length vars) ++ " local variable(s) and " ++ show (length arrays) ++ " local array(s)."
    newFrameSize = frameSize + newFrameSpace

processArrayDecs :: [Variable] -> Scope -> FrameSize -> (Scope, Int)
processArrayDecs ivs scope frameSize = (newScope, frameSpace) where
    addArray (s, f) (i, v) = (Map.insert i (LocalVariable (-(frameSize + f + sizeOfVar v))) s, f + sizeOfVar v)
    (newScope, frameSpace) = foldl addArray (scope, 0) ivs

processVarDecs :: [Variable] -> Scope -> FrameSize -> (Scope, Int)
processVarDecs ivs scope frameSize = (newScope, frameSpace) where
    addVar (s, f) (i, v) = (Map.insert i (LocalVariable (-(frameSize + f + sizeOfVar v))) s, f + sizeOfVar v)
    (newScope, frameSpace) = foldl addVar (scope, 0) ivs

processStatement :: Statement -> Scope -> Strings -> FrameSize -> [Label] -> Bool -> (Code, [Label])
processStatement (ExpressionStmt e) scope strings _ labels _ = (code, labels) where
    code = processExpression e scope strings
processStatement EmptyExpressionStmt _ _ _ labels _ = ([], labels)
processStatement (CompoundStmtStmt c) scope strings frameSize labels isMain = recursion where
    recursion = processCompoundStmt c scope strings frameSize labels isMain
processStatement (IfStmt e s) scope strings frameSize (falseLabel:labels) isMain = (code, remainingLabels) where
    code = conditionCode ++ [testCode, falseJump] ++ bodyCode ++ falseLabelCode
    conditionCode = processExpression e scope strings
    testCode = TestInstruction (SourceRegister Accumulator) (DestinationRegister Accumulator)
    falseJump = JumpZeroInstruction falseLabel
    (bodyCode, remainingLabels) = processStatement s scope strings frameSize labels isMain
    falseLabelCode = [LabelDirective falseLabel]
processStatement (IfElseStmt e s s') scope strings frameSize (falseLabel:trueLabel:labels) isMain = (code, remainingLabels) where
    code = conditionCode ++ testCode ++ falseJump ++ trueCode ++ trueJump ++ falseLabelCode ++ falseCode ++ trueLabelCode
    conditionCode = processExpression e scope strings
    testCode = [TestInstruction (SourceRegister Accumulator) (DestinationRegister Accumulator)]
    falseJump = [JumpZeroInstruction falseLabel]
    trueJump = [JumpInstruction trueLabel]
    (trueCode, falseLabels) = processStatement s scope strings frameSize labels isMain
    (falseCode, remainingLabels) = processStatement s' scope strings frameSize falseLabels isMain
    trueLabelCode = [LabelDirective trueLabel]
    falseLabelCode = [LabelDirective falseLabel]
processStatement (WhileStmt e s) scope strings frameSize (loopLabel:breakLabel:labels) isMain = (code, remainingLabels) where
    code = loopLabelCode ++ conditionCode ++ testCode ++ falseJump ++ bodyCode ++ loopJump ++ breakLabelCode
    loopLabelCode = [LabelDirective loopLabel]
    conditionCode = processExpression e scope strings
    testCode = [TestInstruction (SourceRegister Accumulator) (DestinationRegister Accumulator)]
    falseJump = [JumpZeroInstruction breakLabel]
    (bodyCode, remainingLabels) = processStatement s scope strings frameSize labels isMain
    loopJump = [JumpInstruction loopLabel]
    breakLabelCode = [LabelDirective breakLabel]
processStatement (ReturnStmt e) scope strings _ labels _ = (code, labels) where
    code = expressionCode ++ [stackRestoreInstruction, returnInstruction]
    expressionCode = processExpression e scope strings
    stackRestoreInstruction = MoveInstruction (SourceRegister SaveOne) (DestinationRegister StackPointer)
    returnInstruction = ReturnInstruction
processStatement EmptyReturnStmt _ _ _ labels isMain = (code, labels) where
    code = [stackRestoreInstruction] ++ exitStatusCode ++ [returnInstruction]
    stackRestoreInstruction = MoveInstruction (SourceRegister SaveOne) (DestinationRegister StackPointer)
    exitStatusCode = if isMain
        then [MoveInstruction (SourceImmediate 0) (DestinationRegister Accumulator)]
        else []
    returnInstruction = ReturnInstruction
processStatement (WriteStmt e) scope strings _ labels _ = (code, labels) where
    code = [commentOne] ++ expressionCode ++ [commentTwo, loadFirstArgument, loadFormatString, clearAccumulator, callFunction, commentThree]
    commentOne = Comment "Beginning call to write()"
    expressionCode = processExpression e scope strings
    commentTwo = Comment "Preparing for call to printf"
    formatString = case nodeType e of
        (IntNode, RawNode) -> ".integerWrite"
        (StringNode, RawNode) -> ".stringWrite"
        (VoidNode, _) -> error "Void expression encountered as argument to write statement. This should never happen."
        (_, PointerNode) -> error "Pointer expression encountered as argument to write statement. This should never happen."
        (_, ArrayNode) -> error "Array expression encountered as argument to write statement. This should never happen."
    -- move rax into rsi
    loadFirstArgument = MoveInstruction (SourceRegister Accumulator) (DestinationRegister TempTwo)
    -- move $.WriteIntString into rdi
    loadFormatString = LoadAddressInstruction (SourceLabel formatString) (DestinationRegister TempOne)
    -- move 0 in eax.
    clearAccumulator = ClearInstruction (DestinationRegister Accumulator)
    -- call printf
    callFunction = CallInstruction "printf"
    commentThree = Comment "Ending call to write()"
processStatement WritelnStmt _ _ _ labels _ = (code, labels) where
    code = [commentOne, loadFormatString, clearAccumulator, callFunction, commentTwo]
    commentOne = Comment "Beginning call to writeln()"
    loadFormatString = LoadAddressInstruction (SourceLabel ".newLineWrite") (DestinationRegister TempOne)
    -- Just move the address of WritelnString into rdi,
    clearAccumulator = ClearInstruction (DestinationRegister Accumulator)
    -- move 0 into eax
    callFunction = CallInstruction "printf"
    -- and call printf
    commentTwo = Comment "Ending call to writeln()"

--------------------------------------------------------------------------------

processExpression :: Expression -> Scope -> Strings -> Code
processExpression (SimpleExpression compExp _) scope strings = result where
    result = [commentOne] ++ compExpCode ++ [commentTwo]
    commentOne = Comment "Begin evaluating simple expression"
    compExpCode = processCompExp compExp scope strings
    commentTwo = Comment "End evaluating simple expression"
processExpression (AssignmentExpression i m e n) scope strings = result where
    result = assignmentCode
    expressionCode = processExpression e scope strings
    leftLookup = scope Map.! i
    assignmentCode = case m of
        (RawAssignment) -> processRawAssignment expressionCode leftLookup varSize
        (DereferenceAssignment) -> processDereferenceAssignment expressionCode leftLookup varSize
        (ArrayAssignment index) -> processArrayAssignment expressionCode (processExpression index scope strings) leftLookup varSize
    varSize = sizeOfNode n

processRawAssignment :: Code -> ScopeValue -> Int -> Code
processRawAssignment expressionCode leftLookup varSize = result where
    result = commentOne ++ expressionCode ++ commentTwo ++ assignmentCode ++ commentThree
    commentOne = [Comment "Evaluating expression on right side of raw assignment"]
    commentTwo = [Comment "Moving evaluated expression to left side of raw assignment"]
    moveHalf = case leftLookup of
        (LocalVariable o) -> MoveHalfInstruction (SourceHalfRegister AccumulatorHalf) (frameDestinationHalf o)
        (GlobalVariable l) -> MoveHalfInstruction (SourceHalfRegister AccumulatorHalf) (DestinationHalfLabel l)
        (ArrayParameter _) -> error
            "An array has been encountered as an lvalue for a raw assignment in the code generation phase. This should never happen."
        (Function _) -> error
            "Function encountered as lvalue. This should never happen."
    moveFull = case leftLookup of
        (LocalVariable o) -> MoveInstruction (SourceRegister Accumulator) (frameDestination o)
        (GlobalVariable l) -> MoveInstruction (SourceRegister Accumulator) (DestinationLabel l)
        (ArrayParameter _) -> error
            "An array has been encountered as an lvalue for a raw assignment in the code generation phase. This should never happen."
        (Function _) -> error
            "Function encountered as lvalue. This should never happen."
    assignmentCode = case varSize of
        4 -> [moveHalf]
        8 -> [moveFull]
        size -> error $
            "Cannot store value of size " ++ show size ++ " at raw memory location."
    commentThree = [Comment "Raw assignment completed"]

processDereferenceAssignment :: Code -> ScopeValue -> Int -> Code
processDereferenceAssignment expressionCode leftLookup varSize = result where
    result = commentOne ++ expressionCode ++ commentTwo ++ assignmentCode ++ commentThree
    commentOne = [Comment "Evaluating expression on right side of dereference assignment"]
    commentTwo = [Comment "Moving evaluated expression to left side of dereference assignment"]
    addressInstruction = case leftLookup of
        (LocalVariable o) -> MoveInstruction (frameSource o) (DestinationRegister TempOne)
        (GlobalVariable l) -> MoveInstruction (SourceLabel l) (DestinationRegister TempOne)
        (ArrayParameter _) -> error
            "An array has been encountered as an lvalue for a dereference assignment in the code generation phase. This should never happen."
        (Function _) -> error
            "Function encountered as lvalue. This should never happen."
    moveInstruction = case varSize of
        4 -> MoveHalfInstruction (SourceHalfRegister AccumulatorHalf) (DestinationHalfOffset (Offset TempOne 0))
        8 -> MoveInstruction (SourceRegister Accumulator) (DestinationOffset (Offset TempOne 0))
        size -> error $
            "Cannot store value of size " ++ show size ++ " at dereferenced memory location."
    assignmentCode = [addressInstruction, moveInstruction]
    commentThree = [Comment "Dereference assignment completed"]

processArrayAssignment :: Code -> Code -> ScopeValue -> Int -> Code
processArrayAssignment expressionCode indexCode leftLookup elemSize = result where
    result = commentOne ++ loadAddressCode ++ [pushAddressInstruction] ++ commentTwo ++ expressionCode ++ commentThree ++ [popAddressInstruction, writeAddressInstruction] ++ commentFour
    commentOne = [Comment "Evaluating index of array assignment"]
    -- <indexCode> moves the index value of the array access into the accumulator
    offsetInstruction = case elemSize of
        4 -> ShiftLeftInstruction (SourceImmediate 2) (DestinationRegister Accumulator)
        8 -> ShiftLeftInstruction (SourceImmediate 3) (DestinationRegister Accumulator)
        size -> error $
            "Cannot calculate array offset for elements of size " ++ show size
    -- <offsetCode> multiplies the index value by the size of each element
    commentTwo = [Comment "Evaluating expression on right side of array assignment"]
    commentThree = [Comment "Moving evaluated expression to left side of array assignment"]
    loadBaseInstruction = case leftLookup of
        (LocalVariable o) -> LoadAddressInstruction (frameSource o) (DestinationRegister TempOne)
        (GlobalVariable l) -> LoadAddressInstruction (SourceLabel l) (DestinationRegister TempOne)
        (ArrayParameter o) -> MoveInstruction (frameSource o) (DestinationRegister TempOne)
        (Function _) -> error
            "Function encountered as lvalue. This should never happen."
    -- <loadBaseInstruction> moves the base address of the array into TempOne
    calculateAddressInstruction = AddInstruction (SourceRegister Accumulator) (DestinationRegister TempOne)
    -- <calculateAddressInstruction> moves the address of the array element into TempOne
    loadAddressCode = indexCode ++ [offsetInstruction, loadBaseInstruction, calculateAddressInstruction]
    pushAddressInstruction = PushInstruction (SourceRegister TempOne)
    popAddressInstruction = PopInstruction (DestinationRegister TempOne)
    writeAddressInstruction = case elemSize of
        4 -> MoveHalfInstruction (SourceHalfRegister AccumulatorHalf) (DestinationHalfOffset (Offset TempOne 0))
        8 -> MoveInstruction (SourceRegister Accumulator) (DestinationOffset (Offset TempOne 0))
        size -> error $
            "Cannot store value of size " ++ show size ++ " at array memory location."
    commentFour = [Comment "Array assignment completed"]

processCompExp :: CompExp -> Scope -> Strings -> Code
processCompExp (CompExp e r e' _) scope strings = result where
    rightECode = processE e' scope strings
    pushRightECode = [PushInstruction (SourceRegister Accumulator)]
    leftECode = processE e scope strings
    popRightECode = [PopInstruction (DestinationRegister TempOne)]
    comparison = [CompareHalfInstruction (SourceHalfRegister TempOneHalf) (DestinationHalfRegister AccumulatorHalf)]
    setInstruction = case r of
        LessThanRelOp -> SetLessInstruction
        LessThanOrEqualRelOp -> SetLessEqualInstruction
        EqualRelOp -> SetEqualInstruction
        NotEqualRelOp -> SetNotEqualInstruction
        GreaterThanOrEqualRelOp -> SetGreaterEqualInstruction
        GreaterThanRelOp -> SetGreaterInstruction
    setCode = [setInstruction (DestinationRegister Accumulator)]
    result = rightECode ++ pushRightECode ++ leftECode ++ popRightECode ++ comparison ++ setCode
processCompExp (SimpleExp e _) scope strings = processE e scope strings

processE :: E -> Scope -> Strings -> Code
processE (AddE e a t _) scope strings = result where
    tCode = processT t scope strings
    pushTCode = [PushInstruction (SourceRegister Accumulator)]
    eCode = processE e scope strings
    popTCode = [PopInstruction (DestinationRegister TempOne)]
    operation = case a of
        PlusAddOp -> AddHalfInstruction (SourceHalfRegister TempOneHalf) (DestinationHalfRegister AccumulatorHalf)
        MinusAddOp -> SubHalfInstruction (SourceHalfRegister TempOneHalf) (DestinationHalfRegister AccumulatorHalf)
    result = tCode ++ pushTCode ++ eCode ++ popTCode ++ [operation]
processE (SimpleE t _) scope strings = processT t scope strings

processT :: T -> Scope -> Strings -> Code
processT (MulT t m f _) scope strings = result where
    fCode = processF f scope strings
    pushFCode = [PushInstruction (SourceRegister Accumulator)]
    tCode = processT t scope strings
    popFCode = [PopInstruction (DestinationRegister TempOne)]
    operation = case m of
        TimesMulOp -> MulInstruction (SourceHalfRegister TempOneHalf) (DestinationHalfRegister AccumulatorHalf)
        DivMulOp -> DivInstruction (SourceHalfRegister TempOneHalf) (SourceHalfRegister AccumulatorHalf) (DestinationHalfRegister AccumulatorHalf)
        ModMulOp -> ModInstruction (SourceHalfRegister TempOneHalf) (SourceHalfRegister AccumulatorHalf) (DestinationHalfRegister AccumulatorHalf)
    result = fCode ++ pushFCode ++ tCode ++ popFCode ++ [operation]
processT (SimpleT f _) scope strings = processF f scope strings

processF :: F -> Scope -> Strings -> Code
processF (NegativeF f _) scope strings = fCode ++ [negateResult] where
    fCode = processF f scope strings
    negateResult = NegateHalfInstruction (DestinationHalfRegister AccumulatorHalf)
processF (ReferenceF (RawLValue i _) _) scope _ = result where
    varLookup = scope Map.! i
    source = case varLookup of
        (LocalVariable o) -> frameSource o
        (GlobalVariable l) -> SourceLabel l
        (ArrayParameter _) -> error
            "An array has been encountered as a reference f in the code generation phase. This should never happen."
        (Function _) -> error
            "Function encountered as lvalue. This should never happen."
    result = [LoadAddressInstruction source (DestinationRegister Accumulator)]
processF (ReferenceF (ArrayLValue i e n) _) scope strings = result where
    result = expressionCode ++ [loadBase, calculateOffset, calculateAddress]
    expressionCode = processExpression e scope strings
    -- <expressionCode> calculates the array index value and puts it into the accumulator
    varLookup = scope Map.! i
    source = case varLookup of
        (LocalVariable o) -> frameSource o
        (GlobalVariable l) -> SourceLabel l
        (ArrayParameter _) -> error
            "An array has been encountered as a reference f in the code generation phase. This should never happen."
        (Function _) -> error
            "Function encountered as lvalue. This should never happen."
    calculateOffset = case sizeOfNode n of
        4 -> ShiftLeftInstruction (SourceImmediate 2) (DestinationRegister Accumulator)
        8 -> ShiftLeftInstruction (SourceImmediate 3) (DestinationRegister Accumulator)
        size -> error $
            "Cannot calculate array offset for elements of size " ++ show size
    -- <calculateOffset> multiplies the index by the size of an array element to get the offset from the base address
    loadBase = LoadAddressInstruction source (DestinationRegister TempOne)
    -- <loadBase> moves the address of the base of the array into TempOne
    calculateAddress = AddInstruction (SourceRegister TempOne) (DestinationRegister Accumulator)
    -- <calculateAddress> adds the offset to the base address, returning the address of the array element
processF (DereferenceF factor n) scope strings = factorCode ++ moveResult where
    factorCode = processFactor factor scope strings
    moveHalf = MoveHalfInstruction (SourceHalfOffset (Offset Accumulator 0)) (DestinationHalfRegister AccumulatorHalf)
    moveFull = MoveInstruction (SourceOffset (Offset Accumulator 0)) (DestinationRegister Accumulator)
    moveResult = case sizeOfNode n of
        4 -> [moveHalf]
        8 -> [moveFull]
        size -> error $
            "Cannot move dereferenced factor of size " ++ show size ++ " into accumulator register."
processF (SimpleF factor _) scope strings = processFactor factor scope strings

-- Take in a Factor, output Assembly Code that places the result of computing said
-- Factor into the accumulator register.
processFactor :: Factor -> Scope -> Strings -> Code
processFactor (GroupedFactor e _) scope strings = processExpression e scope strings
processFactor (FunCallFactor f _) scope strings = processFunCall f scope strings
processFactor (ReadFactor _) _ _ = result where
    result = [commentOne, decrementStack] ++ alignStack ++ [loadReadString, loadCallAddress, callScanf, moveResult] ++ unalignStack ++ [incrementStack, commentTwo]
    commentOne = Comment "Beginning call to read()"
    decrementStack = SubInstruction (SourceImmediate 28) (DestinationRegister StackPointer)
    --  Decrement the stack pointer by 28 bytes.
    aligningComment = Comment "Attempting to align stack before call to scanf"
    copyStackPointer = MoveInstruction (SourceRegister StackPointer) (DestinationRegister SaveTwo)
    modStackPointer = AndInstruction (SourceImmediate 15) (DestinationRegister SaveTwo)
    moveStackPointerDown = SubInstruction (SourceRegister SaveTwo) (DestinationRegister StackPointer)
    alignedComment = Comment "Stack should now be aligned."
    alignStack = [aligningComment, copyStackPointer, modStackPointer, moveStackPointerDown, alignedComment]
    --  Align the stack on a 16-byte boundary
    loadReadString = LoadAddressInstruction (SourceLabel ".integerRead") (DestinationRegister TempOne)
    --  Put $.integerRead into TempOne
    loadCallAddress = LoadAddressInstruction (SourceOffset (Offset StackPointer 24)) (DestinationRegister TempTwo)
    --  Put the address 24 bytes below the new stack pointer into TempTwo. Why 24? Because. Just because.
    callScanf = CallInstruction "scanf"
    --  Call scanf
    moveResult = MoveHalfInstruction (SourceHalfOffset (Offset StackPointer 24)) (DestinationHalfRegister AccumulatorHalf)
    --  Move 24(%rsp) into Accumulator.
    unaligningComment = Comment "Attempting to undo stack alignment after call to scanf"
    moveStackPointerUp = AddInstruction (SourceRegister SaveTwo) (DestinationRegister StackPointer)
    unalignedComment = Comment "Stack alignment should now be undone."
    unalignStack = [unaligningComment, moveStackPointerUp, unalignedComment]
    --  Undo previous stack alignment efforts (the SaveTwo register should be preserved across the call to scanf)
    incrementStack = AddInstruction (SourceImmediate 28) (DestinationRegister StackPointer)
    --  Increment the stack pointer by 28 bytes
    commentTwo = Comment "Ending call to read()"
processFactor (DereferenceFactor i n) scope _ = result where
    varLookup = scope Map.! i
    -- <varLookup> should contain the value of <i> in the current scope
    moveHalf = case varLookup of
        (LocalVariable o) -> MoveHalfInstruction (frameSourceHalf o) (DestinationHalfRegister AccumulatorHalf)
        (GlobalVariable l) -> MoveHalfInstruction (SourceHalfLabel l) (DestinationHalfRegister AccumulatorHalf)
        (ArrayParameter _) -> error
            "An array has been encountered as a dereference factor in the code generation phase. This should never happen."
        (Function _) -> error
            "A function call has been encountered as a variable in the code generation phase. This should never happen."
    moveFull = case varLookup of
        (LocalVariable o) -> MoveInstruction (frameSource o) (DestinationRegister Accumulator)
        (GlobalVariable l) -> MoveInstruction (SourceLabel l) (DestinationRegister Accumulator)
        (ArrayParameter _) -> error
            "An array has been encountered as a dereference factor in the code generation phase. This should never happen."
        (Function _) -> error
            "A function call has been encountered as a variable in the code generation phase. This should never happen."
    result = case sizeOfNode n of
        4 -> [moveHalf]
        8 -> [moveFull]
        size -> error $
            "Cannot move dereferenced variable " ++ i ++ " of size " ++ show size ++ " into accumulator register."
processFactor (VarFactor i (_, ArrayNode)) scope _ = [result] where
    varLookup = scope Map.! i
    result = case varLookup of
        (LocalVariable o) -> LoadAddressInstruction (frameSource o) (DestinationRegister Accumulator)
        (GlobalVariable l) -> LoadAddressInstruction (SourceLabel l) (DestinationRegister Accumulator)
        (ArrayParameter o) -> MoveInstruction (frameSource o) (DestinationRegister Accumulator)
        (Function _) -> error
            "A function call has been encountered as an array variable factor in the code generation phase. This should never happen."
processFactor (VarFactor i n) scope _ = result where
    varLookup = scope Map.! i
    moveHalf = case varLookup of
        (LocalVariable o) -> MoveHalfInstruction (frameSourceHalf o) (DestinationHalfRegister AccumulatorHalf)
        (GlobalVariable l) -> MoveHalfInstruction (SourceHalfLabel l) (DestinationHalfRegister AccumulatorHalf)
        (ArrayParameter _) -> error
            "An array has been encountered as a non-array variable factor in the code generation phase. This should never happen."
        (Function _) -> error
            "A function call has been encountered as a variable in the code generation phase. This should never happen."
    moveFull = case varLookup of
        (LocalVariable o) -> MoveInstruction (frameSource o) (DestinationRegister Accumulator)
        (GlobalVariable l) -> MoveInstruction (SourceLabel l) (DestinationRegister Accumulator)
        (ArrayParameter _) -> error
            "An array has been encountered as a non-array variable factor in the code generation phase. This should never happen."
        (Function _) -> error
            "A function call has been encountered as a variable in the code generation phase. This should never happen."
    result = case sizeOfNode n of
        4 -> [moveHalf]
        8 -> [moveFull]
        size -> error $
            "Cannot move variable " ++ i ++ " of size " ++ show size ++ " into accumulator register."
processFactor (ArrayReferenceFactor i e n) scope strings = result where
    result = indexValue ++ [indexShift, base, calculateAddress, moveResult]
    indexValue = processExpression e scope strings
    -- <indexValue> evaluates <e> and leaves its value in the accumulator register
    indexShift = case sizeOfNode n of
        4 -> ShiftLeftInstruction (SourceImmediate 2) (DestinationRegister Accumulator)
        8 -> ShiftLeftInstruction (SourceImmediate 3) (DestinationRegister Accumulator)
        -- TODO: Get rid of this ridiculous copout
        size -> error $
            "Cannot calculate array offset for elements of size " ++ show size
    -- <indexShift> shifts the index left by three bits, to get the offset from the base
    baseLookup = scope Map.! i
    base = case baseLookup of
        (LocalVariable o) -> LoadAddressInstruction (frameSource o) (DestinationRegister TempOne)
        (GlobalVariable l) -> LoadAddressInstruction (SourceLabel l) (DestinationRegister TempOne)
        (ArrayParameter o) -> MoveInstruction (frameSource o) (DestinationRegister TempOne)
        (Function _) -> error
            "A function call has been encountered as a variable in the code generation phase. This should never happen."
    -- <base> should move the base address of the array referenced by <i> into temporary register one
    calculateAddress = AddInstruction (SourceRegister Accumulator) (DestinationRegister TempOne)
    -- <calculateAddress> should move the actual address of the array element into TempOne
    moveResult = MoveInstruction (SourceOffset (Offset TempOne 0)) (DestinationRegister Accumulator)
    -- <moveResult> loads the memory location referenced by TempOne into the accumulator
processFactor (NumberFactor n _) _ _ = [result] where
    result = MoveHalfInstruction (SourceHalfImmediate n) (DestinationHalfRegister AccumulatorHalf)
processFactor (StringFactor s _) _ strings = [result] where
    label = strings Map.! s
    result = LoadAddressInstruction (SourceLabel label) (DestinationRegister Accumulator)

--  Before the call the caller pushes the arguments onto the stack in reverse order (last argument first, first
-- argument last) then pushes the fp onto the stack and makes the call.
--  At the return the caller pops the stack into the fp, restoring fp into what it was before the call. The
-- caller then increments the stack by enough to pop the arguments that were pushed there. Any return value
-- will be found in eax or rax.
processFunCall :: FunCall -> Scope -> Strings -> Code
processFunCall (FunCall i es _) scope strings = result where
    result = pushFramePointerCode ++ argsCode ++ decrementStackCode ++ functionCall ++ restoreStackCode ++ popFramePointerCode
    -- pushArgCode n = case sizeOfNode n of
    --     4 -> [PushHalfInstruction (SourceHalfRegister AccumulatorHalf)]
    --     8 -> [PushInstruction (SourceRegister Accumulator)]
    --     size -> error $
    --         "Cannot push argument of size " ++ show size ++ " to stack."
    -- processArg e = processExpression e scope strings ++ pushArgCode (nodeType e)
    -- argsCode = concatMap processArg $ reverse es
    pushExpression (o, c) e = let argCode = processExpression e scope strings in
        case sizeOfNode $ nodeType e of
            4 -> (o - 4, c ++ argCode ++ [MoveHalfInstruction (SourceHalfRegister AccumulatorHalf) (DestinationHalfOffset (Offset StackPointer (o - 4)))])
            8 -> (o - 8, c ++ argCode ++ [MoveInstruction (SourceRegister Accumulator) (DestinationOffset (Offset StackPointer (o - 8)))])
            size -> error $
                "Cannot write argument to function " ++ i ++ " of size " ++ show size ++ " to stack."
    (argsOffset, argsCode) = foldl pushExpression (0, []) (reverse es)
    argsSize = negate argsOffset
    decrementStackCode = [SubInstruction (SourceImmediate argsSize) (DestinationRegister StackPointer)]
    pushFramePointerCode = [PushInstruction (SourceRegister SaveOne)]
    functionCall = [CallInstruction i]
    popFramePointerCode = [PopInstruction (DestinationRegister SaveOne)]
    restoreStackCode = if argsSize > 0
        then [AddInstruction (SourceImmediate argsSize) (DestinationRegister StackPointer)]
        else []

--------------------------------------------------------------------------------

setNub :: (Ord a) => [a] -> [a]
setNub = Set.toList . Set.fromList

frameSource :: Int -> Source
frameSource = SourceOffset . Offset SaveOne

frameDestination :: Int -> Destination
frameDestination = DestinationOffset . Offset SaveOne

frameSourceHalf :: Int -> SourceHalf
frameSourceHalf = SourceHalfOffset . Offset SaveOne

frameDestinationHalf :: Int -> DestinationHalf
frameDestinationHalf = DestinationHalfOffset . Offset SaveOne

-- Size required to store the given node type (in bytes)
sizeOfNode :: NodeType -> Int
sizeOfNode (IntNode, RawNode) = 4
sizeOfNode _ = 8

sizeOfVar :: VarType -> Int
sizeOfVar (IntVar, ArrayVar n) = 4 * n
sizeOfVar (StringVar, ArrayVar n) = 8 * n
sizeOfVar v = sizeOfNode $ varNodeType v
