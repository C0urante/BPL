module Instruction where

type Symbol = String
type Label =  String

data Register = Accumulator
              | StackPointer
              | TempOne
              | TempTwo
              | TempThree
              | TempFour
              | TempFive
              | TempSix
              | SaveOne
              | SaveTwo
              | SaveThree
              | SaveFour
              | SaveFive
              | SaveSix
                deriving (Eq)

data RegisterHalf = AccumulatorHalf
--                  SHOULD NEVER USE THIS
--                | StackPointerHalf
                  | TempOneHalf
                  | TempTwoHalf
                  | TempThreeHalf
                  | TempFourHalf
                  | TempFiveHalf
                  | TempSixHalf
                  | SaveOneHalf
                  | SaveTwoHalf
                  | SaveThreeHalf
                  | SaveFourHalf
                  | SaveFiveHalf
                  | SaveSixHalf
                    deriving (Eq)

instance Show Register where
    show r = "%" ++ regName where
        regName = case r of
            Accumulator      -> "rax"
            StackPointer     -> "rsp"
            TempOne          -> "rdi"
            TempTwo          -> "rsi"
            TempThree        -> "rdx"
            TempFour         -> "rcx"
            TempFive         -> "r8"
            TempSix          -> "r9"
            SaveOne          -> "rbx"
            SaveTwo          -> "rbp"
            SaveThree        -> "r10"
            SaveFour         -> "r13"
            SaveFive         -> "r14"
            SaveSix          -> "r15"

instance Show RegisterHalf where
    show r = "%" ++ regName where
        regName = case r of
            AccumulatorHalf  -> "eax"
--          SHOULD NEVER USE THIS
--          StackPointerHalf -> "esp"
            TempOneHalf      -> "edi"
            TempTwoHalf      -> "edi"
            TempThreeHalf    -> "edx"
            TempFourHalf     -> "ecx"
            TempFiveHalf     -> "r8d"
            TempSixHalf      -> "r9d"
            SaveOneHalf      -> "ebx"
            SaveTwoHalf      -> "ebp"
            SaveThreeHalf    -> "r10d"
            SaveFourHalf     -> "r13d"
            SaveFiveHalf     -> "r14d"
            SaveSixHalf      -> "r15d"

data Offset = Offset Register Integer
              deriving (Eq)
instance Show Offset where
    show (Offset register offset) = show offset ++ "(" ++ show register ++ ")"

data Source = SourceRegister Register
            | SourceOffset Offset
            | SourceImmediate Int
            | SourceLabel Label
              deriving (Eq)
data SourceHalf = SourceHalfRegister RegisterHalf
                | SourceHalfOffset Offset
                | SourceHalfImmediate Int
                | SourceHalfLabel Label
                  deriving (Eq)

instance Show Source where
    show (SourceRegister r) =  show r
    show (SourceOffset o) =    show o
    show (SourceImmediate n) = "$" ++ show n
    show (SourceLabel l) =     "$" ++ l
instance Show SourceHalf where
    show (SourceHalfRegister r) =  show r
    show (SourceHalfOffset o) =    show o
    show (SourceHalfImmediate n) = "$" ++ show n
    show (SourceHalfLabel l) =     "$" ++ l

data Destination = DestinationRegister Register
                 | DestinationOffset Offset
                   deriving (Eq)
data DestinationHalf = DestinationHalfRegister RegisterHalf
                     | DestinationHalfOffset Offset
                       deriving (Eq)

instance Show Destination where
    show (DestinationRegister r) = show r
    show (DestinationOffset o) =   show o
instance Show DestinationHalf where
    show (DestinationHalfRegister r) = show r
    show (DestinationHalfOffset o) =   show o

data AssemblyLine = DataDirective
                  | CommDirective Symbol Integer Integer
                  | ReadOnlyDirective
                  | StringDirective Symbol String
                  | TextDirective
                  | GlobalDirective Symbol
                  | MoveInstruction Source Destination
                  | MoveHalfInstruction SourceHalf DestinationHalf
                  | LoadAddressInstruction Source Destination
                  | ClearInstruction Destination
                  | ClearHalfInstruction DestinationHalf
                  | PushInstruction Source
                  | PopInstruction Destination
                  | JumpInstruction Label
                  | CompareInstruction Source Destination
                  | CompareHalfInstruction SourceHalf DestinationHalf
                  | JumpEqualInstruction Label
                  | JumpNotEqualInstruction Label
                  | JumpLessInstruction Label
                  | JumpLessEqualInstruction Label
                  | JumpGreaterInstruction Label
                  | JumpGreaterEqualInstruction Label
                  | JumpZeroInstruction Label
                  | CallInstruction Label
                  | ReturnInstruction
                  | AddInstruction Source Destination
                  | AddHalfInstruction SourceHalf DestinationHalf
                  | SubInstruction Source Destination
                  | SubHalfInstruction SourceHalf DestinationHalf
                  | MulInstruction SourceHalf Destination
                  | DivInstruction SourceHalf SourceHalf
                  | ModInstruction SourceHalf SourceHalf
                    deriving (Eq)

instance Show AssemblyLine where
    show DataDirective =                    ".data"
    show (CommDirective i b a) =            ".comm\t" ++ i ++ ", " ++ show b ++ ", " ++ show a
    show ReadOnlyDirective =                ".section\t.rodata"
    show (StringDirective i s) =            i ++ ":\t.string " ++ show s
    show TextDirective =                    ".text"
    show (GlobalDirective i) =              ".globl\t" ++ i
    show (MoveInstruction s d) =            "movq\t" ++ show s ++ ", " ++ show d
    show (MoveHalfInstruction s d) =        "movl\t" ++ show s ++ ", " ++ show d
    show (ClearInstruction d) =             "clrq\t" ++ show d
    show (ClearHalfInstruction d) =         "clrl\t" ++ show d
    show (LoadAddressInstruction s d) =     "leaq\t" ++ show s ++ ", " ++ show d
    show (PushInstruction s) =              "push\t" ++ show s
    show (PopInstruction d) =               "pop\t"  ++ show d
    show (JumpInstruction l) =              "jump\t" ++ l
    show (CompareInstruction s d) =         "cmpl\t" ++ show s ++ ", " ++ show d
    show (CompareHalfInstruction s d) =     "cmpl\t" ++ show s ++ ", " ++ show d
    show (JumpEqualInstruction l) =         "je\t"   ++ l
    show (JumpNotEqualInstruction l) =      "jne\t"  ++ l
    show (JumpLessInstruction l) =          "jl\t"   ++ l
    show (JumpLessEqualInstruction l) =     "jle\t"  ++ l
    show (JumpGreaterInstruction l) =       "jg\t"   ++ l
    show (JumpGreaterEqualInstruction l) =  "jge\t"  ++ l
    show (JumpZeroInstruction l) =          "jz\t"   ++ l
    show (CallInstruction l) =              "call\t" ++ l
    show ReturnInstruction =                "ret"
    show (AddInstruction s d) =             "addq\t" ++ show s ++ ", " ++ show d
    show (AddHalfInstruction s d) =         "addl\t" ++ show s ++ ", " ++ show d
    show (SubInstruction s d) =             "subq\t" ++ show s ++ ", " ++ show d
    show (SubHalfInstruction s d) =         "subl\t" ++ show s ++ ", " ++ show d
    show (MulInstruction s d) =             "imul\t" ++ show s ++ ", " ++ show d
    show (DivInstruction s s') =            showDivModInstruction s s'
    show (ModInstruction s s') =            showDivModInstruction s s'

showDivModInstruction :: SourceHalf -> SourceHalf -> String
showDivModInstruction dividend divisor = line1 ++ line2 ++ line3 ++ line4 ++ line5 where
    line1 = "movl\t" ++ show dividend ++ ", " ++ "%ebp\n"
    line2 = "movl\t" ++ show divisor  ++ ", " ++ "%eax\n"
    line3 = "cltq\n"
    line4 = "clto\n"
    line5 = "idivl\t%ebp"
