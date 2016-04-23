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
            TempTwoHalf      -> "esi"
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

data Offset = Offset Register Int
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
    show (SourceLabel l) =     l
instance Show SourceHalf where
    show (SourceHalfRegister r) =  show r
    show (SourceHalfOffset o) =    show o
    show (SourceHalfImmediate n) = "$" ++ show n
    show (SourceHalfLabel l) =     l

data Destination = DestinationRegister Register
                 | DestinationOffset Offset
                 | DestinationLabel Label
                   deriving (Eq)
data DestinationHalf = DestinationHalfRegister RegisterHalf
                     | DestinationHalfOffset Offset
                     | DestinationHalfLabel Label
                       deriving (Eq)

instance Show Destination where
    show (DestinationRegister r) = show r
    show (DestinationOffset o) =   show o
    show (DestinationLabel l) =    l
instance Show DestinationHalf where
    show (DestinationHalfRegister r) = show r
    show (DestinationHalfOffset o) =   show o
    show (DestinationHalfLabel l) =    l

data AssemblyLine = DataDirective
                  | CommDirective Symbol Int Int
                  | ReadOnlyDirective
                  | StringDirective Symbol String
                  | TextDirective
                  | GlobalDirective Symbol
                  | LabelDirective Symbol
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
                  | SetLessInstruction Destination
                  | SetLessEqualInstruction Destination
                  | SetEqualInstruction Destination
                  | SetNotEqualInstruction Destination
                  | SetGreaterEqualInstruction Destination
                  | SetGreaterInstruction Destination
                  | TestInstruction Source Destination
                  | TestHalfInstruction SourceHalf DestinationHalf
                  | JumpLessInstruction Label
                  | JumpLessEqualInstruction Label
                  | JumpEqualInstruction Label
                  | JumpNotEqualInstruction Label
                  | JumpGreaterEqualInstruction Label
                  | JumpGreaterInstruction Label
                  | JumpZeroInstruction Label
                  | CallInstruction Label
                  | ReturnInstruction
                  | AddInstruction Source Destination
                  | AddHalfInstruction SourceHalf DestinationHalf
                  | SubInstruction Source Destination
                  | SubHalfInstruction SourceHalf DestinationHalf
                  | NegateInstruction Destination
                  | NegateHalfInstruction DestinationHalf
                  | MulInstruction SourceHalf DestinationHalf
                  | DivInstruction SourceHalf SourceHalf DestinationHalf
                  | ModInstruction SourceHalf SourceHalf DestinationHalf
                  | ShiftLeftInstruction Source Destination
                  | Comment String
                    deriving (Eq)

instance Show AssemblyLine where
    show DataDirective =                    ".data"
    show (CommDirective i b a) =            "\t.comm\t" ++ i ++ ", " ++ show b ++ ", " ++ show a
    show ReadOnlyDirective =                ".section\t.rodata"
    show (StringDirective i s) =            "\t" ++ i ++ ":\t.string " ++ show s
    show TextDirective =                    ".text"
    show (GlobalDirective i) =              "\t.globl\t" ++ i
    show (LabelDirective s) =               s ++ ":"
    show (MoveInstruction s d) =            "\tmovq\t" ++ show s ++ ", " ++ show d
    show (MoveHalfInstruction s d) =        "\tmovl\t" ++ show s ++ ", " ++ show d
    show (ClearInstruction d) =             "\tclrq\t" ++ show d
    show (ClearHalfInstruction d) =         "\tclrl\t" ++ show d
    show (LoadAddressInstruction s d) =     "\tleaq\t" ++ show s ++ ", " ++ show d
    show (PushInstruction s) =              "\tpush\t" ++ show s
    show (PopInstruction d) =               "\tpop \t" ++ show d
    show (JumpInstruction l) =              "\tjmp \t" ++ l
    show (CompareInstruction s d) =         "\tcmpl\t" ++ show s ++ ", " ++ show d
    show (CompareHalfInstruction s d) =     "\tcmpl\t" ++ show s ++ ", " ++ show d
    show (SetLessInstruction d) =           showRelOpInstruction "l"  d
    show (SetLessEqualInstruction d) =      showRelOpInstruction "le" d
    show (SetEqualInstruction d) =          showRelOpInstruction "e"  d
    show (SetNotEqualInstruction d) =       showRelOpInstruction "ne" d
    show (SetGreaterEqualInstruction d) =   showRelOpInstruction "ge" d
    show (SetGreaterInstruction d) =        showRelOpInstruction "g"  d
    show (TestInstruction s d) =            "\ttestq\t" ++ show s ++ ", " ++ show d
    show (TestHalfInstruction s d) =        "\ttestl\t" ++ show s ++ ", " ++ show d
    show (JumpLessInstruction l) =          "\tjl  \t" ++ l
    show (JumpLessEqualInstruction l) =     "\tjle \t" ++ l
    show (JumpEqualInstruction l) =         "\tje  \t" ++ l
    show (JumpNotEqualInstruction l) =      "\tjne \t" ++ l
    show (JumpGreaterEqualInstruction l) =  "\tjge \t" ++ l
    show (JumpGreaterInstruction l) =       "\tjg  \t" ++ l
    show (JumpZeroInstruction l) =          "\tjz  \t" ++ l
    show (CallInstruction l) =              "\tcall\t" ++ l
    show ReturnInstruction =                "\tret"
    show (AddInstruction s d) =             "\taddq\t" ++ show s ++ ", " ++ show d
    show (AddHalfInstruction s d) =         "\taddl\t" ++ show s ++ ", " ++ show d
    show (SubInstruction s d) =             "\tsubq\t" ++ show s ++ ", " ++ show d
    show (SubHalfInstruction s d) =         "\tsubl\t" ++ show s ++ ", " ++ show d
    show (NegateInstruction d) =            "\tnegq\t" ++ show d
    show (NegateHalfInstruction d) =        "\tnegl\t" ++ show d
    show (MulInstruction s d) =             "\timul\t" ++ show s ++ ", " ++ show d
    show (DivInstruction s s' d) =          showDivModInstruction s s' d Division
    show (ModInstruction s s' d) =          showDivModInstruction s s' d Modulus
    show (ShiftLeftInstruction s d) =       "\tsalq\t" ++ show s ++ ", " ++ show d
    show (Comment s) =                      "# " ++ s

showRelOpInstruction :: String -> Destination -> String
showRelOpInstruction op destination = setInstruction ++ expandInstruction where
    setInstruction = "\tset" ++ op ++ "\t%al\n"
    expandInstruction = "\tmovzbq\t%al, " ++ show destination

data DivOperator = Division | Modulus

showDivModInstruction :: SourceHalf -> SourceHalf -> DestinationHalf -> DivOperator -> String
showDivModInstruction dividend divisor destination op = result where
    result = moveDividend ++ moveDivisor ++ extendToQuad ++ extendToOctet ++ divide ++ moveToDestination
    moveDividend = "\tmovl\t" ++ show dividend ++ ", " ++ "%ebp\n"
    moveDivisor = "\tmovl\t" ++ show divisor  ++ ", " ++ "%eax\n"
    extendToQuad = "\tcltq\n"
    extendToOctet = "\tcqto\n"
    divide = "\tidivl\t%ebp\n"
    resultRegister = case op of
        Division -> "%eax"
        Modulus -> "%edx"
    moveToDestination = "\tmovl\t" ++ resultRegister ++ ", " ++ show destination
