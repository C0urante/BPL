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
-- assembly label used for globals and functions
data ScopeValue =
    LocalVariable Int |
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

-- As of right now, the only purpose for this is to generate directives for string
-- literals used in the program and in calls to printf/scanf, and track the labels
-- given to string literals.
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
    stringWrite =  StringDirective ".stringWrite"  "%s"
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
    extractFromFactor _ = []

-- Generate assembly code to give each global variable an address in the data
-- section with the appropriate amount of data allocated, and keep track of these
-- addresses and the identifiers they correspond to.
dataSection :: Program -> (Code, Scope)
dataSection p = result where
    globals = extractGlobalVariables p
    isArray (_, (_, ArrayVar _)) = True
    isArray _  = False
    (arrays, vars) = partition isArray globals
    (arrayCode, arrayScope) = arraySection arrays Map.empty
    (varCode, fullScope) = varSection vars arrayScope
    result = ([DataDirective] ++ arrayCode ++ varCode, fullScope)

extractGlobalVariables :: Program -> [Variable]
extractGlobalVariables (Program ds) = map extractVariable $ filter isVarDec ds where
    isVarDec (VarDec _ _) = True
    isVarDec (FunDec _ _ _ _) = False
    extractVariable (VarDec i t) = (i, t)

arraySection :: [Variable] -> Scope -> (Code, Scope)
arraySection vs s = (code, scope) where
    arrayLine ((_, (_, ArrayVar n)), l) = CommDirective l (n * 8) 8
    addArrayToScope s' ((i, _), l) = Map.insert i (GlobalVariable l) s'
    arraysAndLabels = zip vs labels
    code = map arrayLine arraysAndLabels
    scope = foldl addArrayToScope s arraysAndLabels
    labels = labelStream "A"

varSection :: [Variable] -> Scope -> (Code, Scope)
varSection vs s = (code, scope) where
    varLine (_, l) = CommDirective l 8 8
    addVarToScope s' ((i, _), l) = Map.insert i (GlobalVariable l) s'
    varsAndLabels = zip vs labels
    code = map varLine varsAndLabels
    scope = foldl addVarToScope s varsAndLabels
    labels = labelStream "V"

textSection :: Program -> Scope -> Strings -> Code
textSection p scope strings = result where
    result = [TextDirective, GlobalDirective "main"]
    newScope = undefined
    -- Have to add Global variables corresponding to each function
    functions = extractFunctions p

extractFunctions :: Program -> [Function]
extractFunctions (Program ds) = map extractFunction $ filter isFunDec ds where
    isFunDec (FunDec _ _ _ _) = True
    isFunDec (VarDec _ _) = False
    extractFunction (FunDec i r p c) = (i, r, p, c)

--------------------------------------------------------------------------------

--  Before the call the caller pushes the arguments onto the stack in reverse order (last argument first, first
-- argument last) then pushes the fp onto the stack and makes the call.
--  At the start of the call the callee puts the current stack pointer, which points at the return address, into
-- the frame pointer. The callee then decrements the stack pointer by 8 times the number of local variables,
-- to allocate room for all of the local variables.
--  At the return the callee puts into eax or rax any return value, then pops off the stack any temporary data
-- saved there, then increments the stack pointer to de-allocate the local variables. The callee then executes
-- a ret .
--  The return instruction pops the return address off the stack.
--  At the return the caller pops the stack into the fp, restoring fp into what it was before the call. The
-- caller then increments the stack by enough to pop the arguments that were pushed there. Any return value
-- will be found in eax or rax.
processFunction :: Function -> Scope -> Strings -> Code
processFunction (i, t, p, c) scope strings = undefined where
    labels = labelStream i
    newScope = undefined
    --  Need to add the location of each argument relative to the frame pointer
    -- to the scope.

processCompoundStmt :: CompoundStmt -> Scope -> Strings -> FrameSize -> [Label] -> (Code, [Label])
processCompoundStmt (CompoundStmt ld ss) scope strings frameSize labels = undefined where
    (newScope, newFrameSize, stackDecrementCode) = processLocalDecs ld scope frameSize

--  Return the new scope and frame size after making room for each new variable,
-- as well as code used to decrement the stack pointer.
processLocalDecs :: LocalDecs -> Scope -> FrameSize -> (Scope, FrameSize, Code)
processLocalDecs (LocalDecs []) scope frameSize = (scope, frameSize, []) -- If there are no local decs, don't bother.
processLocalDecs (LocalDecs ivs) scope frameSize = (newScope, newFrameSize, code) where
    isArray (_, (_, ArrayVar _)) = True
    isArray _  = False
    (arrays, vars) = partition isArray ivs
    (arrayScope, arraySpace) = processArrayDecs arrays scope frameSize
    (newScope, varSpace) = processVarDecs vars arrayScope (frameSize + arraySpace)
    newFrameSpace = arraySpace + varSpace
    newFrameSize = frameSize + arraySpace + varSpace
    code = [SubInstruction (SourceImmediate newFrameSpace) (DestinationRegister StackPointer)]

processArrayDecs :: [Variable] -> Scope -> FrameSize -> (Scope, Int)
processArrayDecs ivs scope frameSize = (newScope, frameSpace) where
    addArray (s, f) (i, (_, ArrayVar n)) = (Map.insert i (LocalVariable (-f)) s, f + (8 * n))
    (newScope, frameSpace) = foldl addArray (scope, frameSize) ivs

processVarDecs :: [Variable] -> Scope -> FrameSize -> (Scope, Int)
processVarDecs ivs scope frameSize = (newScope, frameSpace) where
    addVar (s, f) (i, _) = (Map.insert i (LocalVariable (-f)) s, f - 8)
    (newScope, frameSpace) = foldl addVar (scope, frameSize) ivs

processStatement :: Statement -> Scope -> Strings -> FrameSize -> [Label] -> (Code, [Label])
processStatement (ExpressionStmt e) scope strings _ labels = (code, labels) where
    code = processExpression e scope strings
processStatement EmptyExpressionStmt _ _ _ labels = ([], labels)
processStatement (CompoundStmtStmt c) scope strings frameSize labels = recursion where
    recursion = processCompoundStmt c scope strings frameSize labels
processStatement (IfStmt e s) scope strings frame labelsSize = undefined
processStatement (IfElseStmt e s s') scope strings frameSize labels = undefined
processStatement (WhileStmt e s) scope strings frameSize labels = undefined
processStatement (ReturnStmt e) scope strings _ labels = (code, labels) where
    code = expressionCode ++ returnCode
    expressionCode = processExpression e scope strings
    returnCode = [ReturnInstruction]
processStatement EmptyReturnStmt _ _ _ labels = (code, labels) where
    code = [ReturnInstruction]
processStatement (WriteStmt e) scope strings _ labels = (code, labels) where
    code = expressionCode ++ [loadFirstArgument, loadFormatString, clearAccumulator, callFunction]
    expressionCode = processExpression e scope strings
    formatString = case nodeType e of
        (IntNode, RawNode) -> ".integerWrite"
        (StringNode, RawNode) -> ".stringWrite"
        (VoidNode, _) -> error "Void expression encountered as argument to write statement. This should never happen."
        (_, PointerNode) -> error "Pointer expression encountered as argument to write statement. This should never happen."
        (_, ArrayNode) -> error "Array expression encountered as argument to write statement. This should never happen."
    -- move eax into esi
    loadFirstArgument = MoveHalfInstruction (SourceHalfRegister AccumulatorHalf) (DestinationHalfRegister TempTwoHalf)
    -- move $.WriteIntString into rdi
    loadFormatString = MoveInstruction (SourceLabel formatString) (DestinationRegister TempOne)
    -- move 0 in eax.
    clearAccumulator = ClearInstruction (DestinationRegister Accumulator)
    -- call printf
    callFunction = CallInstruction "printf"
processStatement WritelnStmt _ _ _ labels = (code, labels) where
    code = [loadFormatString, clearAccumulator, callFunction]
    loadFormatString = MoveInstruction (SourceLabel ".newLineWrite") (DestinationRegister TempOne)
    -- Just move $WritelnString into rdi,
    clearAccumulator = ClearInstruction (DestinationRegister Accumulator)
    -- move 0 into eax
    callFunction = CallInstruction "printf"
    -- and call printf

--------------------------------------------------------------------------------

processExpression :: Expression -> Scope -> Strings -> Code
processExpression (SimpleExpression compExp _) scope strings = processCompExp compExp scope strings
processExpression (AssignmentExpression i m e _) scope strings = undefined where
    expressionCode = processExpression e scope strings
    leftLookup = scope Map.! i

processCompExp :: CompExp -> Scope -> Strings -> Code
processCompExp (CompExp e r e' _) scope strings = result where
    leftECode = processE e scope strings
    pushLeftECode = [PushInstruction (SourceRegister Accumulator)]
    rightECode = processE e' scope strings
    popLeftECode = [PopInstruction (DestinationRegister TempOne)]
    comparison = [CompareHalfInstruction (SourceHalfRegister TempOneHalf) (DestinationHalfRegister AccumulatorHalf)]
    setInstruction = case r of
        LessThanRelOp -> SetLessInstruction
        LessThanOrEqualRelOp -> SetLessEqualInstruction
        EqualRelOp -> SetEqualInstruction
        NotEqualRelOp -> SetNotEqualInstruction
        GreaterThanOrEqualRelOp -> SetGreaterEqualInstruction
        GreaterThanRelOp -> SetGreaterInstruction
    setCode = [setInstruction (DestinationHalfRegister AccumulatorHalf)]
    result = leftECode ++ pushLeftECode ++ rightECode ++ popLeftECode ++ comparison ++ setCode
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
        TimesMulOp -> MulInstruction (SourceHalfRegister TempOneHalf) (DestinationRegister Accumulator)
        DivMulOp -> DivInstruction (SourceHalfRegister TempOneHalf) (SourceHalfRegister AccumulatorHalf) (DestinationHalfRegister AccumulatorHalf)
        ModMulOp -> ModInstruction (SourceHalfRegister TempOneHalf) (SourceHalfRegister AccumulatorHalf) (DestinationHalfRegister AccumulatorHalf)
    result = fCode ++ pushFCode ++ tCode ++ popFCode ++ [operation]
processT (SimpleT f _) scope strings = processF f scope strings

processF :: F -> Scope -> Strings -> Code
processF (NegativeF f _) scope strings = fCode ++ [negateResult] where
    fCode = processF f scope strings
    negateResult = NegateHalfInstruction (DestinationHalfRegister AccumulatorHalf)
processF (ReferenceF factor _) scope strings = undefined
    -- WTF?!?!?!?!?! Doesn't this allow F nodes like &5?!?!?!?!?!?!
processF (DereferenceF factor _) scope strings = factorCode ++ [moveResult] where
    factorCode = processFactor factor scope strings
    moveResult = MoveInstruction (SourceOffset (Offset Accumulator 0)) (DestinationRegister Accumulator)
processF (SimpleF factor _) scope strings = processFactor factor scope strings

-- Take in a Factor, output Assembly Code that places the result of computing said
-- Factor into the accumulator register.
processFactor :: Factor -> Scope -> Strings -> Code
processFactor (GroupedFactor e _) scope strings = processExpression e scope strings
processFactor (FunCallFactor f _) scope strings = processFunCall f scope strings
processFactor (ReadFactor _) _ _ = result where
    result = [decrementStack, loadCallAddress, loadReadString, callScanf, moveResult, incrementStack]
    decrementStack = SubInstruction (SourceImmediate 40) (DestinationRegister StackPointer)
    --  Decrement the stack pointer by 40 bytes.
    loadCallAddress = LoadAddressInstruction (SourceOffset (Offset StackPointer 24)) (DestinationRegister TempTwo)
    --  Put the address 24 bytes below the new stack pointer into TempTwo. Why 24? Because. Just because.
    loadReadString = MoveInstruction (SourceLabel ".integerRead") (DestinationRegister TempOne)
    --  Put $.integerRead into TempOne
    callScanf = CallInstruction "scanf"
    --  Call scanf
    moveResult = MoveHalfInstruction (SourceHalfOffset (Offset StackPointer 24)) (DestinationHalfRegister AccumulatorHalf)
    --  Move 24(%rsp) into Accumulator.
    incrementStack = AddInstruction (SourceImmediate 40) (DestinationRegister StackPointer)
    --  Increment the stack pointer by 40 bytes
processFactor (DereferenceFactor i _) scope _ = [address, moveResult] where
    varLookup = scope Map.! i
    -- <varLookup> should contain the value of <i> in the current scope
    address = case varLookup of
        (LocalVariable o) -> MoveInstruction (SourceOffset (Offset TempOne o)) (DestinationRegister Accumulator)
        (GlobalVariable l) -> MoveInstruction (SourceLabel l) (DestinationRegister Accumulator)
        _ -> error "A function call has been encountered as a variable in the code generation phase. This should never happen."
    --  <address> should handle cases of varLookup being either local (offset from frame)
    -- or global (raw addess), and load the value of either into the accumulator
    moveResult = MoveInstruction (SourceOffset (Offset Accumulator 0)) (DestinationRegister Accumulator)
    -- <result> moves the memory location referenced by the accumulator into the accumulator itself
processFactor (VarFactor i _) scope _ = [result] where
    varLookup = scope Map.! i
    result = case varLookup of
        (LocalVariable o) -> MoveInstruction (SourceOffset (Offset TempOne o)) (DestinationRegister Accumulator)
        (GlobalVariable l) -> MoveInstruction (SourceLabel l) (DestinationRegister Accumulator)
        _ -> error "A function call has been encountered as a variable in the code generation phase. This should never happen."
processFactor (ArrayReferenceFactor i e _) scope strings = result where
    result = indexValue ++ [base, address, moveResult]
    indexValue = processExpression e scope strings
    -- indexValue moves the evaluated value of <e> into the accumulator register
    baseLookup = scope Map.! i
    base = case baseLookup of
        (LocalVariable o) -> AddInstruction (SourceOffset (Offset SaveOne o)) (DestinationRegister TempOne)
        (GlobalVariable l) -> MoveInstruction (SourceLabel l) (DestinationRegister TempOne)
        _ -> error "A function call has been encountered as a variable in the code generation phase. This should never happen."
    -- <base> should move the base address of the array referenced by <i> into temporary register one
    address = AddInstruction (SourceRegister TempOne) (DestinationRegister Accumulator)
    -- <address> should move the actual address of the array element into the accumulator
    moveResult = MoveInstruction (SourceOffset (Offset Accumulator 0)) (DestinationRegister Accumulator)
    -- <moveResult> loads the memory location referenced by the accumulator into the accumulator itself
processFactor (NumberFactor n _) _ _ = [result] where
    result = MoveInstruction (SourceImmediate n) (DestinationRegister Accumulator)
processFactor (StringFactor s _) _ strings = [result] where
    result = MoveInstruction (SourceLabel label) (DestinationRegister Accumulator)
    label = strings Map.! s

--  Before the call the caller pushes the arguments onto the stack in reverse order (last argument first, first
-- argument last) then pushes the fp onto the stack and makes the call.
processFunCall :: FunCall -> Scope -> Strings -> Code
processFunCall (FunCall i es n) = undefined

--------------------------------------------------------------------------------

setNub :: (Ord a) => [a] -> [a]
setNub = Set.toList . Set.fromList
