module Compiler where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Types hiding (Scope)
import Instruction

--  Integers correspond to frame offsets for locals and addresses for globals,
-- and Strings correspond to labels for functions
data ScopeValue =
    LocalVariable Integer |
    GlobalVariable Integer |
    Function Label
type Scope = Map.Map Identifier ScopeValue
type Strings = Map.Map String Label
type Code = [AssemblyLine]

compile :: Program -> Code
compile p = result where
    (readOnlyCode, _, readRemainderLabels) = readOnlySection p labelStream
    (dataCode, _, _) = dataSection p readRemainderLabels
    result = readOnlyCode ++ dataCode

labelStream :: [Label]
labelStream = map ((".LC" ++) . leftPad 2 '0' . show) ([0..] :: [Int]) where
    leftPad :: Int -> Char -> String -> String
    leftPad l c s = let stringLength = length s in
        if stringLength >= l
            then s
            else replicate (l - stringLength) c ++ s

-- As of right now, the only purpose for this is to generate directives for string
-- literals used in the program and in calls to printf/scanf, and track the labels
-- given to string literals.
readOnlySection :: Program -> [Label] -> (Code, Strings, [Label])
readOnlySection p ls = (code, strings, labels) where
    stringLiterals = extractStringLiterals p
    (stringCode, strings, labels) = stringLabels stringLiterals ls
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
stringLabels :: [String] -> [Label] -> (Code, Strings, [Label])
stringLabels ss ls = (code, strings, labels) where
    stringLine (s, l) = StringDirective l s
    addStringToMap m (s, l) = Map.insert s l m
    initMap = Map.empty
    stringsAndLabels = zip ss ls
    code = map stringLine stringsAndLabels
    strings = foldl addStringToMap initMap stringsAndLabels
    labels = drop (length ss) ls

-- Generate assembly code to give each global variable an address in the data
-- section with the appropriate amount of data allocated, and keep track of these
-- addresses and the identifiers they correspond to.
dataSection :: Program -> [Label] -> (Code, Scope, [Label])
dataSection = undefined

globalVariables :: Program -> [Declaration]
globalVariables (Program ds) = filter isVarDec ds where
    isVarDec (VarDec _ _) = True
    isVarDec (FunDec _ _ _ _) = False

functions :: Program -> [Declaration]
functions (Program ds) = filter isFunDec ds where
    isFunDec (FunDec _ _ _ _) = True
    isFunDec (VarDec _ _) = False

-- Find all string literals contained in a Program
extractStringLiterals :: Program -> [String]
extractStringLiterals p = concatMap extractFromFunDec $ functions p where
    extractFromFunDec (FunDec _ _ _ c) = setNub $ extractFromCompoundStmt c
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

--------------------------------------------------------------------------------

processProgram :: Program -> Code
processProgram (Program ds) = undefined

processDeclaration :: Declaration -> Code
processDeclaration (VarDec i (r, m)) = undefined
processDeclaration (FunDec i r ps c) = undefined

processCompoundStmt :: CompoundStmt -> Code
processCompoundStmt (CompoundStmt ld ss) = undefined

processLocalDecs :: LocalDecs -> Code
processLocalDecs (LocalDecs ivs) = undefined

processStatement :: Statement -> Code
processStatement (ExpressionStmt e) = undefined
processStatement (EmptyExpressionStmt) = undefined
processStatement (CompoundStmtStmt c) = undefined
processStatement (IfStmt e s) = undefined
processStatement (IfElseStmt e s s') = undefined
processStatement (WhileStmt e s) = undefined
processStatement (ReturnStmt e) = undefined
processStatement (EmptyReturnStmt) = undefined
processStatement (WriteStmt e) = undefined
processStatement (WritelnStmt) = undefined

--------------------------------------------------------------------------------

processExpression :: Expression -> Code
processExpression = undefined

processCompExp :: CompExp -> Code
processCompExp (CompExp e r e' n) = undefined
processCompExp (SimpleExp e n) = undefined

processE :: E -> Code
processE (AddE e a t n) = undefined
processE (SimpleE t n) = undefined

processT :: T -> Code
processT (MulT t m f n) = undefined
processT (SimpleT f n) = undefined

processF :: F -> Code
processF (NegativeF f n) = undefined
processF (ReferenceF factor n) = undefined
processF (DereferenceF factor n) = undefined
processF (SimpleF factor n) = undefined

-- Take in a Factor, output Assembly Code that places the result of computing said
-- Factor into the accumulator register.
processFactor :: Factor -> Code
processFactor (GroupedFactor e _) = processExpression e
processFactor (FunCallFactor f _) = undefined
processFactor (ReadFactor _) = result where
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
processFactor (DereferenceFactor i _) = undefined where
    result = LoadAddressInstruction address (DestinationRegister Accumulator)
    varLookup = undefined
    -- <varLookup> should contain the value of <i> in the current scope
    address = undefined
    -- <address> should handle cases of varLookup being either local (offset from frame) or global (raw addess)
processFactor (VarFactor i _) = undefined
processFactor (ArrayReferenceFactor i e _) = result where
    result = indexValue ++ [base, address, moveResult]
    indexValue = processExpression e
    -- indexValue moves the evaluated value of <e> into the accumulator register
    base = undefined
    -- <base> should move the base address of the array referenced by <i> into temporary register one
    address = AddInstruction (SourceRegister TempOne) (DestinationRegister Accumulator)
    moveResult = LoadAddressInstruction (SourceRegister Accumulator) (DestinationRegister Accumulator)
processFactor (NumberFactor n _) = [result] where
    result = MoveInstruction (SourceImmediate n) (DestinationRegister Accumulator)
processFactor (StringFactor s _) = [result] where
    result = MoveInstruction (SourceLabel label) (DestinationRegister Accumulator)
    label = undefined
    -- <label> should be the label corresponding to the string literal <s>

processFunCall :: FunCall -> Code
processFunCall (FunCall i es n) = undefined

--------------------------------------------------------------------------------

setNub :: (Ord a) => [a] -> [a]
setNub = Set.toList . Set.fromList
