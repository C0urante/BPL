module Optimizer where

import Types
import Analyzer (statementHasReturn)

import Data.Maybe
import Data.Int (Int32)

optimize :: Program -> Program
optimize = filterLiveCode . foldConstants

--------------------------------------------------------------------------------

-- Evaluate any expressions whose results are known at compile time
foldConstants :: Program -> Program
foldConstants (Program ds) = Program $ map foldConstantsDeclaration ds

foldConstantsDeclaration :: Declaration -> Declaration
foldConstantsDeclaration d = case d of
    (VarDec _ _) -> d
    (FunDec i r ps c) -> FunDec i r ps $ foldConstantsCompoundStmt c

foldConstantsCompoundStmt :: CompoundStmt -> CompoundStmt
foldConstantsCompoundStmt (CompoundStmt lds ss) = CompoundStmt lds $ map foldConstantsStatement ss

foldConstantsStatement :: Statement -> Statement
foldConstantsStatement (ExpressionStmt e) = result where
    result = ExpressionStmt $ foldConstantsExpression e
foldConstantsStatement (EmptyExpressionStmt) = result where
    result = EmptyExpressionStmt
foldConstantsStatement (CompoundStmtStmt c) = result where
    result = CompoundStmtStmt $ foldConstantsCompoundStmt c
foldConstantsStatement (IfStmt e s) = result where
    result = case constCondition of
        (Just n) -> if n /= 0
            then body
            else EmptyExpressionStmt
        (Nothing) -> IfStmt condition body
    condition = foldConstantsExpression e
    body = foldConstantsStatement s
    constCondition = extractConstant condition
foldConstantsStatement (IfElseStmt e s s') = result where
    result = case constCondition of
        (Just n) -> if n /= 0
            then body
            else body'
        (Nothing) -> IfElseStmt condition body body'
    condition = foldConstantsExpression e
    body = foldConstantsStatement s
    body' = foldConstantsStatement s'
    constCondition = extractConstant condition
foldConstantsStatement (WhileStmt e s) = result where
    result = case constCondition of
        (Just n) -> if n /= 0
            then WhileStmt (toConstant n) body
            else EmptyExpressionStmt
        Nothing -> WhileStmt condition body
    condition = foldConstantsExpression e
    body = foldConstantsStatement s
    constCondition = extractConstant condition
foldConstantsStatement (ReturnStmt e) = result where
    result = ReturnStmt $ foldConstantsExpression e
foldConstantsStatement (EmptyReturnStmt) = result where
    result = EmptyReturnStmt
foldConstantsStatement (WriteStmt e) = result where
    result = WriteStmt $ foldConstantsExpression e
foldConstantsStatement (WritelnStmt) = result where
    result = WritelnStmt

foldConstantsExpression :: Expression -> Expression
foldConstantsExpression (SimpleExpression c n) = result where
    result = SimpleExpression c' n
    c' = foldConstantsCompExp c
foldConstantsExpression (AssignmentExpression i m e n) = result where
    result = AssignmentExpression i m e' n
    e' = foldConstantsExpression e

foldConstantsCompExp :: CompExp -> CompExp
foldConstantsCompExp (SimpleExp e n) = result where
    result = SimpleExp e' n
    e' = foldConstantsE e
foldConstantsCompExp (CompExp e r e' _) = result where
    result = case r of
        LessThanRelOp -> foldConstantsLessExp e'' e'''
        LessThanOrEqualRelOp -> foldConstantsLessEqualExp e'' e'''
        EqualRelOp -> foldConstantsEqualExp e'' e'''
        NotEqualRelOp -> foldConstantsNotEqualExp e'' e'''
        GreaterThanRelOp -> foldConstantsGreaterExp e'' e'''
        GreaterThanOrEqualRelOp -> foldConstantsGreaterEqualExp e'' e'''
    e'' = foldConstantsE e
    e''' = foldConstantsE e'

foldConstantsLessExp :: E -> E -> CompExp
foldConstantsLessExp e e' = result where
    result = case (constLeft, constRight) of
        (Just n, Just n') -> if n < n'
            then toConstant 1
            else toConstant 0
        _ -> nonOptimizedResult
    left = foldConstantsE e
    right = foldConstantsE e'
    nonOptimizedResult = CompExp left LessThanRelOp right rawIntNode
    constLeft = extractConstant left
    constRight = extractConstant right

foldConstantsLessEqualExp :: E -> E -> CompExp
foldConstantsLessEqualExp e e' = result where
    result = case (constLeft, constRight) of
        (Just n, Just n') -> if n <= n'
            then toConstant 1
            else toConstant 0
        _ -> nonOptimizedResult
    left = foldConstantsE e
    right = foldConstantsE e'
    nonOptimizedResult = CompExp left LessThanOrEqualRelOp right rawIntNode
    constLeft = extractConstant left
    constRight = extractConstant right

foldConstantsEqualExp :: E -> E -> CompExp
foldConstantsEqualExp e e' = result where
    result = case (constLeft, constRight) of
        (Just n, Just n') -> if n == n'
            then toConstant 1
            else toConstant 0
        _ -> nonOptimizedResult
    left = foldConstantsE e
    right = foldConstantsE e'
    nonOptimizedResult = CompExp left EqualRelOp right rawIntNode
    constLeft = extractConstant left
    constRight = extractConstant right

foldConstantsNotEqualExp :: E -> E -> CompExp
foldConstantsNotEqualExp e e' = result where
    result = case (constLeft, constRight) of
        (Just n, Just n') -> if n /= n'
            then toConstant 1
            else toConstant 0
        _ -> nonOptimizedResult
    left = foldConstantsE e
    right = foldConstantsE e'
    nonOptimizedResult = CompExp left EqualRelOp right rawIntNode
    constLeft = extractConstant left
    constRight = extractConstant right

foldConstantsGreaterEqualExp :: E -> E -> CompExp
foldConstantsGreaterEqualExp e e' = result where
    result = case (constLeft, constRight) of
        (Just n, Just n') -> if n >= n'
            then toConstant 1
            else toConstant 0
        _ -> nonOptimizedResult
    left = foldConstantsE e
    right = foldConstantsE e'
    nonOptimizedResult = CompExp left GreaterThanOrEqualRelOp right rawIntNode
    constLeft = extractConstant left
    constRight = extractConstant right

foldConstantsGreaterExp :: E -> E -> CompExp
foldConstantsGreaterExp e e' = result where
    result = case (constLeft, constRight) of
        (Just n, Just n') -> if n > n'
            then toConstant 1
            else toConstant 0
        _ -> nonOptimizedResult
    left = foldConstantsE e
    right = foldConstantsE e'
    nonOptimizedResult = CompExp left GreaterThanRelOp right rawIntNode
    constLeft = extractConstant left
    constRight = extractConstant right

-- Constants propagate right (up) when being folded by the helper; however, this
-- can potentially result in a slightly unwieldy 'optimization' where an expression
-- like "3 - x" is replaced by "0 - x + 3"; as a result, we look for that here
-- and move the constant (which, if there is one, is now the right child of
-- the E node) to the bottom of the tree. In the example, this would mean returning
-- "3 - x" at the end instead.
foldConstantsE :: E -> E
foldConstantsE e = result where
    result = case foldedE of
        (SimpleE _ _) -> foldedE
        (AddE e' PlusAddOp t' _) ->
            let right = extractConstant t'
                lowest = extractConstant $ extractLowestChild e' in
                    case (lowest, right) of
                        (Just 0, Just n) -> replaceLowestChild n e'
                        _ -> foldedE
        (AddE e' MinusAddOp t' _) ->
            let right = extractConstant t'
                lowest = extractConstant $ extractLowestChild e' in
                    case (lowest, right) of
                        (Just 0, Just n) -> replaceLowestChild (negate n) e'
                        _ -> foldedE
    foldedE = foldConstantsHelperE e
    extractLowestChild (SimpleE t n) = SimpleE t n
    extractLowestChild (AddE child _ _ _) = extractLowestChild child
    replaceLowestChild n (SimpleE _ _) = toConstant n
    replaceLowestChild n (AddE child o t n') = AddE (replaceLowestChild n child) o t n'

foldConstantsHelperE :: E -> E
foldConstantsHelperE (SimpleE t n) = result where
    result = SimpleE t' n
    t' = foldConstantsT t
foldConstantsHelperE (AddE e PlusAddOp t _) = result where
    result = case (constLeft, constRight) of
        (Just n, Just n') -> toConstant $ n + n'
        (Just 0, Nothing) -> SimpleE right rawIntNode
        (Just n, Nothing) -> AddE (SimpleE right rawIntNode) PlusAddOp (toConstant n) rawIntNode
        (Nothing, Just 0) -> left
        (Nothing, Just n) -> case left of
            (AddE e' PlusAddOp t' _) -> let leftRight = extractConstant t' in
                case leftRight of
                    (Just n') -> AddE e' PlusAddOp (toConstant $ n + n') rawIntNode
                    (Nothing) -> nonOptimizedResult
            (AddE e' MinusAddOp t' _) -> let leftRight = extractConstant t' in
                case leftRight of
                    (Just n') -> AddE e' PlusAddOp (toConstant $ n - n') rawIntNode
                    (Nothing) -> nonOptimizedResult
            (SimpleE _ _) -> nonOptimizedResult
        (Nothing, Nothing) -> case left of
            (AddE e' PlusAddOp t' _) -> let leftRight = extractConstant t' in
                case leftRight of
                    (Just n') -> AddE (AddE e' PlusAddOp right rawIntNode) PlusAddOp (toConstant n') rawIntNode
                    (Nothing) -> nonOptimizedResult
            (AddE e' MinusAddOp t' _) -> let leftRight = extractConstant t' in
                case leftRight of
                    (Just n') -> AddE (AddE e' PlusAddOp right rawIntNode) MinusAddOp (toConstant n') rawIntNode
                    (Nothing) -> nonOptimizedResult
            (SimpleE _ _) -> nonOptimizedResult
    left = foldConstantsHelperE e
    right = foldConstantsT t
    nonOptimizedResult = AddE left PlusAddOp right rawIntNode
    constLeft = extractConstant left
    constRight = extractConstant right
foldConstantsHelperE (AddE e MinusAddOp t _) = result where
    result = case (constLeft, constRight) of
        (Just n, Just n') -> toConstant $ n - n'
        (Just n, Nothing) -> AddE (AddE (toConstant 0) MinusAddOp right rawIntNode) PlusAddOp (toConstant n) rawIntNode
        (Nothing, Just 0) -> left
        (Nothing, Just n) -> case left of
            (AddE e' PlusAddOp t' _) -> let leftRight = extractConstant t' in
                case leftRight of
                    (Just n') -> AddE e' MinusAddOp (toConstant $ n - n') rawIntNode
                    (Nothing) -> nonOptimizedResult
            (AddE e' MinusAddOp t' _) -> let leftRight = extractConstant t' in
                case leftRight of
                    (Just n') -> AddE e' MinusAddOp (toConstant $ n + n') rawIntNode
                    (Nothing) -> nonOptimizedResult
            (SimpleE _ _) -> AddE left MinusAddOp right rawIntNode
        (Nothing, Nothing) -> case left of
            (AddE e' PlusAddOp t' _) -> let leftRight = extractConstant t' in
                case leftRight of
                    (Just n') -> AddE (AddE e' MinusAddOp right rawIntNode) PlusAddOp (toConstant n') rawIntNode
                    (Nothing) -> nonOptimizedResult
            (AddE e' MinusAddOp t' _) -> let leftRight = extractConstant t' in
                case leftRight of
                    (Just n') -> AddE (AddE e' MinusAddOp right rawIntNode) MinusAddOp (toConstant n') rawIntNode
                    (Nothing) -> nonOptimizedResult
            (SimpleE _ _) -> nonOptimizedResult
    left = foldConstantsHelperE e
    right = foldConstantsT t
    nonOptimizedResult = AddE left MinusAddOp right rawIntNode
    constLeft = extractConstant left
    constRight = extractConstant right

foldConstantsT :: T -> T
foldConstantsT (SimpleT f n) = result where
    result = SimpleT f' n
    f' = foldConstantsF f
foldConstantsT (MulT t ModMulOp f _) = result where
    result = case (constLeft, constRight) of
        (Just n, Just n') -> if n' /= 0
            then toConstant (n `rem` n')
            else nonOptimizedResult
        (Nothing, Just 1) -> toConstant 0
        _ -> nonOptimizedResult
    left = foldConstantsT t
    right = foldConstantsF f
    nonOptimizedResult = MulT left ModMulOp right rawIntNode
    constLeft = extractConstant left
    constRight = extractConstant right
foldConstantsT (MulT t DivMulOp f _) = result where
    result = case (constLeft, constRight) of
        (Just n, Just n') -> if n' /= 0
            then toConstant (n `quot` n')
            else nonOptimizedResult
        (Nothing, Just 1) -> left
        (Nothing, Nothing) -> case left of
            (MulT t' DivMulOp f' _) -> let leftRight = extractConstant f' in
                case leftRight of
                    (Just n') -> MulT (MulT t' DivMulOp right rawIntNode) DivMulOp (toConstant n') rawIntNode
                    (Nothing) -> nonOptimizedResult
            _ -> nonOptimizedResult
        _ -> nonOptimizedResult
    left = foldConstantsT t
    right = foldConstantsF f
    nonOptimizedResult = MulT left DivMulOp right rawIntNode
    constLeft = extractConstant left
    constRight = extractConstant right
foldConstantsT (MulT t TimesMulOp f _) = result where
    result = case (constLeft, constRight) of
        (Just n, Just n') -> toConstant $ n * n'
        (Just 1, Nothing) -> SimpleT right rawIntNode
        (Just n, Nothing) -> MulT (SimpleT right rawIntNode) TimesMulOp (toConstant n) rawIntNode
        (Nothing, Just 1) -> left
        (Nothing, Just n) -> case left of
            (MulT t' TimesMulOp f' _) -> let leftRight = extractConstant f' in
                case leftRight of
                    (Just n') -> MulT t' TimesMulOp (toConstant $ n * n') rawIntNode
                    (Nothing) -> nonOptimizedResult
            _ -> nonOptimizedResult
        (Nothing, Nothing) -> case left of
            (MulT t' TimesMulOp f' _) -> let leftRight = extractConstant f' in
                case leftRight of
                    (Just n') -> MulT (MulT t' TimesMulOp right rawIntNode) TimesMulOp (toConstant n') rawIntNode
                    _ -> nonOptimizedResult
            _ -> nonOptimizedResult
    left = foldConstantsT t
    right = foldConstantsF f
    nonOptimizedResult = MulT left TimesMulOp right rawIntNode
    constLeft = extractConstant left
    constRight = extractConstant right

foldConstantsF :: F -> F
foldConstantsF (SimpleF factor n) = result where
    result = SimpleF factor' n
    factor' = foldConstantsFactor factor
foldConstantsF (ReferenceF l n) = result where
    result = ReferenceF l' n
    l' = foldConstantsLValue l
foldConstantsF (DereferenceF factor n) = result where
    result = DereferenceF factor' n
    factor' = foldConstantsFactor factor
foldConstantsF (NegativeF f _) = result where
    result = case constF of
        (Just n) -> SimpleF (toConstant $ negate n) rawIntNode
        (Nothing) -> nonOptimizedResult
    f' = foldConstantsF f
    constF = extractConstant f'
    nonOptimizedResult = NegativeF f rawIntNode

foldConstantsFactor :: Factor -> Factor
foldConstantsFactor (NumberFactor n' n) = result where
    result = NumberFactor n' n
foldConstantsFactor (StringFactor s n) = result where
    result = StringFactor s n
foldConstantsFactor (ReadFactor n) = result where
    result = ReadFactor n
foldConstantsFactor (VarFactor i n) = result where
    result = VarFactor i n
foldConstantsFactor (DereferenceFactor i n) = result where
    result = DereferenceFactor i n
foldConstantsFactor (FunCallFactor f n) = result where
    result = FunCallFactor f' n
    f' = foldConstantsFunCall f
foldConstantsFactor (ArrayReferenceFactor i e n) = result where
    result = ArrayReferenceFactor i e' n
    e' = foldConstantsExpression e
foldConstantsFactor (GroupedFactor e n) = result where
    result = case constE of
        (Just n') -> NumberFactor n' n
        (Nothing) -> nonOptimizedResult
    e' = foldConstantsExpression e
    nonOptimizedResult = GroupedFactor e' n
    constE = extractConstant e'

foldConstantsLValue :: LValue -> LValue
foldConstantsLValue (RawLValue i n) = result where
    result = RawLValue i n
foldConstantsLValue (PointerLValue i n) = result where
    result = PointerLValue i n
foldConstantsLValue (ArrayLValue i e n) = result where
    result = ArrayLValue i e' n
    e' = foldConstantsExpression e

foldConstantsFunCall :: FunCall -> FunCall
foldConstantsFunCall (FunCall i es n) = result where
    result = FunCall i es' n
    es' = map foldConstantsExpression es

rawIntNode :: NodeType
rawIntNode = (IntNode, RawNode)

class ConstantNode a where
    extractConstant :: a -> Maybe Int32
    toConstant :: Int32 -> a

instance ConstantNode Expression where
    extractConstant e = case e of
        (AssignmentExpression _ _ _ _) -> Nothing
        (SimpleExpression c _) -> extractConstant c
    toConstant n = SimpleExpression c rawIntNode where
        c = toConstant n

instance ConstantNode CompExp where
    extractConstant c = case c of
        (CompExp _ _ _ _) -> Nothing
        (SimpleExp e _) -> extractConstant e
    toConstant n = SimpleExp e rawIntNode where
        e = toConstant n

instance ConstantNode E where
    extractConstant e = case e of
        (AddE _ _ _ _) -> Nothing
        (SimpleE t _) -> extractConstant t
    toConstant n = SimpleE t rawIntNode where
        t = toConstant n

instance ConstantNode T where
    extractConstant t = case t of
        (MulT _ _ _ _) -> Nothing
        (SimpleT f _) -> extractConstant f
    toConstant n = SimpleT f rawIntNode where
        f = toConstant n

instance ConstantNode F where
    extractConstant f = case f of
        (NegativeF f' _) -> fmap negate $ extractConstant f'
        (ReferenceF _ _) -> Nothing
        (DereferenceF _ _) -> Nothing
        (SimpleF factor _) -> extractConstant factor
    toConstant n = SimpleF factor rawIntNode where
        factor = toConstant n

instance ConstantNode Factor where
    extractConstant (GroupedFactor e _) = extractConstant e
    extractConstant (FunCallFactor _ _) = Nothing
    extractConstant (ReadFactor _) = Nothing
    extractConstant (DereferenceFactor _ _) = Nothing
    extractConstant (VarFactor _ _) = Nothing
    extractConstant (ArrayReferenceFactor _ _ _) = Nothing
    extractConstant (NumberFactor n _) = Just n
    extractConstant (StringFactor _ _) = Nothing
    toConstant n = NumberFactor n rawIntNode

--------------------------------------------------------------------------------

-- Remove code that is guaranteed never to be reached from the program
filterLiveCode :: Program -> Program
filterLiveCode (Program ds) = Program $ map liveCodeDeclaration ds

liveCodeDeclaration :: Declaration -> Declaration
liveCodeDeclaration d = case d of
    (VarDec _ _) -> d
    (FunDec i r ps c) -> FunDec i r ps $ liveCodeCompoundStmt c

liveCodeCompoundStmt :: CompoundStmt -> CompoundStmt
liveCodeCompoundStmt (CompoundStmt lds ss) = result where
    result = CompoundStmt lds liveStatements
    liveStatements = map liveCodeStatement reachableStatements
    reachableStatements = case returnAndAfterStatements of
        returnStatement:_ -> normalStatements ++ [returnStatement]
        [] -> normalStatements
    (normalStatements, returnAndAfterStatements) = break statementHasReturn ss

liveCodeStatement :: Statement -> Statement
liveCodeStatement (CompoundStmtStmt c) = CompoundStmtStmt $ liveCodeCompoundStmt c
liveCodeStatement s = s
