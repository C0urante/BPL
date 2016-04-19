module Compiler where

import qualified Data.Map as Map
import qualified Types
import Instruction

data Variable = StringVariable String | MemoryVariable Int
type Identifier = String
type Scope = Map.Map Identifier Variable
type Code = [AssemblyLine]

compile :: Types.Program -> Code
compile = undefined

labelStream :: [Label]
labelStream = map ((".LC" ++) . leftPad 2 '0' . show) ([0..] :: [Int]) where
    leftPad :: Int -> Char -> String -> String
    leftPad l c s = let stringLength = length s in
        if stringLength >= l
            then s
            else replicate (l - stringLength) c ++ s

writeLabels :: Code
writeLabels = [newLineWrite, stringWrite, integerWrite] where
    newLineWrite = StringDirective ".newLineWrite" "\n"
    stringWrite =  StringDirective ".stringWrite"  "%s"
    integerWrite = StringDirective ".integerWrite" "%d "

readLabel :: Code
readLabel = [StringDirective ".integerRead" "%d"]

globalVariables :: Types.Program -> [Types.Declaration]
globalVariables (Types.Program ds) = filter isVarDec ds where
    isVarDec (Types.VarDec _ _) = True
    isVarDec (Types.FunDec _ _ _ _) = False

functions :: Types.Program -> [Types.Declaration]
functions (Types.Program ds) = filter isFunDec ds where
    isFunDec (Types.FunDec _ _ _ _) = True
    isFunDec (Types.VarDec _ _) = False

strings :: [Types.Declaration] -> [String]
strings = concatMap extractFromFunDec where
    extractFromFunDec (Types.FunDec _ _ _ c) = extractFromCompoundStmt c
    extractFromCompoundStmt (Types.CompoundStmt _ ss) = concatMap extractFromStatement ss
    extractFromStatement (Types.ExpressionStmt e) = extractFromExpression e
    extractFromStatement (Types.CompoundStmtStmt c) = extractFromCompoundStmt c
    extractFromStatement (Types.IfStmt e s) = extractFromExpression e ++ extractFromStatement s
    extractFromStatement (Types.IfElseStmt e s s') = extractFromExpression e ++ extractFromStatement s ++ extractFromStatement s'
    extractFromStatement (Types.WhileStmt e s) = extractFromExpression e ++ extractFromStatement s
    extractFromStatement (Types.ReturnStmt e) = extractFromExpression e
    extractFromStatement (Types.WriteStmt e) = extractFromExpression e
    extractFromStatement _ = []
    extractFromExpression (Types.AssignmentExpression _ _ e _) = extractFromExpression e
    extractFromExpression (Types.SimpleExpression c _) = extractFromCompExp c
    extractFromCompExp (Types.SimpleExp e _) = extractFromE e
    extractFromCompExp _ = []
    extractFromE (Types.SimpleE t _) = extractFromT t
    extractFromE _ = []
    extractFromT (Types.SimpleT f _) = extractFromF f
    extractFromT _ = []
    extractFromF (Types.SimpleF f _) = extractFromFactor f
    extractFromF _ = []
    extractFromFactor (Types.GroupedFactor e _) = extractFromExpression e
    extractFromFactor (Types.StringFactor s _) = [s]
    extractFromFactor _ = []

processExpression :: Types.Expression -> Code
processExpression = undefined

-- Take in a Factor, output Assembly Code that places the result of computing said
-- Factor into the accumulator register.
processFactor :: Types.Factor -> Code
processFactor (Types.GroupedFactor e _) = processExpression e
processFactor (Types.FunCallFactor f _) = undefined
--  Decrement the stack pointer by 40 bytes.
--  Put the address 24 bytes below the new stack pointer into TempTwo. Why 24? Because. Just because.
--  Put $.integerRead into TempOne
--  Call scanf
--  Move 24(%rsp) into Accumulator. read() is an expression in BPL, and all expressions leave their results
-- in Accumulator or AccumulatorHalf.
--  Increment the stack pointer by 40 bytes
processFactor (Types.ReadFactor _) = result where
    result = [decrementStack, loadCallAddress, loadReadString, callScanf, moveResult, incrementStack]
    decrementStack = SubInstruction (SourceImmediate 40) (DestinationRegister StackPointer)
    loadCallAddress = LoadAddressInstruction (SourceOffset (Offset StackPointer 24)) (DestinationRegister TempTwo)
    loadReadString = MoveInstruction (SourceLabel ".integerRead") (DestinationRegister TempOne)
    callScanf = CallInstruction "scanf"
    moveResult = MoveHalfInstruction (SourceHalfOffset (Offset StackPointer 24)) (DestinationHalfRegister AccumulatorHalf)
    incrementStack = AddInstruction (SourceImmediate 40) (DestinationRegister StackPointer)
processFactor (Types.DereferenceFactor i _) = undefined
processFactor (Types.VarFactor i _) = undefined
processFactor (Types.ArrayReferenceFactor i e _) = undefined where
    index = processExpression e
processFactor (Types.NumberFactor n _) = result where
    result = [MoveInstruction (SourceImmediate n) (DestinationRegister Accumulator)]
processFactor (Types.StringFactor s _) = undefined
