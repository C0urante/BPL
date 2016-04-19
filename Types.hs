module Types where

import qualified Data.Map as Map

type LineNumber = Int

data VarMetaType =
    RawVar |
    PointerVar |
    ArrayVar
    deriving (Eq, Show)

data VarRawType =
    IntVar |
    StringVar
    deriving (Eq, Show)

data FunReturnType =
    VoidFun |
    IntFun |
    StringFun
    deriving (Eq, Show)

data DecType =
    FunDecType FunType |
    VarDecType VarType
    deriving (Eq, Show)

data NodeRawType =
    VoidNode |
    IntNode |
    StringNode
    deriving (Eq, Show)

data NodeMetaType =
    RawNode |
    PointerNode |
    ArrayNode
    deriving (Eq, Show)

type Identifier = String
type Environment = Map.Map Identifier DecType

emptyEnvironment :: Environment
emptyEnvironment = Map.empty

data Scope =
    GlobalScope Environment |
    NestedScope Environment Scope
    deriving (Eq, Show)

emptyScope :: Scope
emptyScope = GlobalScope emptyEnvironment

addToScope :: Scope -> Identifier -> DecType -> Scope
addToScope (GlobalScope e) i t = result where
    result = if Map.member i e
        then error $ "Duplicate global declarations corresponding to identifier " ++ i
        else GlobalScope (Map.insert i t e)
addToScope (NestedScope e s) i t = NestedScope (Map.insert i t e) s

lookup :: Scope -> Identifier -> LineNumber -> DecType
lookup (GlobalScope e) i l   = case Map.lookup i e of
    (Just t) -> t
    Nothing -> error $ "Line " ++ show l ++ ": Reference to undefined identifier " ++ i
lookup (NestedScope e s) i l = case Map.lookup i e of
    (Just t) -> t
    Nothing -> Types.lookup s i l

contains :: Scope -> Identifier -> Bool
contains (GlobalScope e) i   = Map.member i e
contains (NestedScope e s) i = Map.member i e || contains s i

type VarType = (VarRawType, VarMetaType)
type FunType = (FunReturnType, [VarType])
type NodeType = (NodeRawType, NodeMetaType)

funReturn :: FunType -> FunReturnType
funReturn = fst
funArgs :: FunType -> [VarType]
funArgs = snd

varRaw :: VarType -> VarRawType
varRaw = fst
varMeta :: VarType -> VarMetaType
varMeta = snd

nodeRaw :: NodeType -> NodeRawType
nodeRaw = fst
nodeMeta :: NodeType -> NodeMetaType
nodeMeta = snd

showVar :: VarType -> String
showVar (t, m) = metaType m ++ " " ++ rawType t where
    rawType IntVar = "integer"
    rawType StringVar = "string"
    metaType RawVar = "raw"
    metaType PointerVar = "pointer to"
    metaType ArrayVar = "array of"

showNode :: NodeType -> String
showNode (r, m) = metaType ++ " " ++ rawType where
    rawType = case r of
        VoidNode -> "void"
        IntNode -> "integer"
        StringNode -> "string"
    metaType = case m of
        RawNode -> "raw"
        PointerNode -> "pointer to"
        ArrayNode -> "array of"

class TypedNode a where
    nodeType :: a -> NodeType

varNodeType :: VarType -> NodeType
varNodeType (r, m) = (raw, meta) where
    raw = case r of
        IntVar -> IntNode
        StringVar -> StringNode
    meta = case m of
        RawVar -> RawNode
        PointerVar -> PointerNode
        ArrayVar -> ArrayNode

funNodeType :: FunType -> NodeType
funNodeType (r, _) = (raw, RawNode) where
    raw = case r of
        VoidFun -> VoidNode
        IntFun -> IntNode
        StringFun -> StringNode

data AssignmentMetaType =
    RawAssignment |
    DereferenceAssignment |
    ArrayAssignment TypedExpression
    deriving (Eq, Show)

data TypedProgram =
    TypedProgram [TypedDeclaration]
    deriving (Eq, Show)

data TypedDeclaration =
    TypedVarDec Identifier VarType |
    TypedFunDec Identifier FunReturnType [(Identifier, VarType)] TypedCompoundStmt
    deriving (Eq, Show)

data TypedCompoundStmt =
    TypedCompoundStmt TypedLocalDecs [TypedStatement]
    deriving (Eq, Show)

data TypedLocalDecs =
    TypedLocalDecs [(Identifier, VarType)]
    deriving (Eq, Show)

data TypedStatement =
    TypedExpressionStmt TypedExpression |
    TypedEmptyExpressionStmt |
    TypedCompoundStmtStmt TypedCompoundStmt |
    TypedIfStmt TypedExpression TypedStatement |
    TypedIfElseStmt TypedExpression TypedStatement TypedStatement |
    TypedWhileStmt TypedExpression TypedStatement |
    TypedReturnStmt TypedExpression |
    TypedEmptyReturnStmt |
    TypedWriteStmt TypedExpression |
    TypedWritelnStmt
    deriving (Eq, Show)

data TypedExpression =
    TypedAssignmentExpression Identifier AssignmentMetaType TypedExpression NodeType |
    TypedSimpleExpression TypedCompExp NodeType
    deriving (Eq, Show)

instance TypedNode TypedExpression where
    nodeType (TypedAssignmentExpression _ _ _ n) = n
    nodeType (TypedSimpleExpression _ n) = n

data TypedCompExp =
    TypedCompExp TypedE TypedRelOp TypedE NodeType |
    TypedSimpleExp TypedE NodeType
    deriving (Eq, Show)

instance TypedNode TypedCompExp where
    nodeType (TypedCompExp _ _ _ n) = n
    nodeType (TypedSimpleExp _ n) = n

data TypedRelOp =
    TypedLessThanOrEqualRelOp |
    TypedLessThanRelOp |
    TypedEqualRelOp |
    TypedNotEqualRelOp |
    TypedGreaterThanRelOp |
    TypedGreaterThanOrEqualRelOp
    deriving (Eq, Show)

data TypedE =
    TypedAddE TypedE TypedAddOp TypedT NodeType |
    TypedSimpleE TypedT NodeType
    deriving (Eq, Show)

instance TypedNode TypedE where
    nodeType (TypedAddE _ _ _ n) = n
    nodeType (TypedSimpleE _ n) = n

data TypedAddOp =
    TypedPlusAddOp |
    TypedMinusAddOp
    deriving (Eq, Show)

data TypedT =
    TypedMulT TypedT TypedMulOp TypedF NodeType |
    TypedSimpleT TypedF NodeType
    deriving (Eq, Show)

instance TypedNode TypedT where
    nodeType (TypedMulT _ _ _ n) = n
    nodeType (TypedSimpleT _ n) = n

data TypedMulOp =
    TypedTimesMulOp |
    TypedDivMulOp |
    TypedModMulOp
    deriving (Eq, Show)

data TypedF =
    TypedNegativeF TypedF NodeType |
    TypedReferenceF TypedFactor NodeType |
    TypedDereferenceF TypedFactor NodeType |
    TypedSimpleF TypedFactor NodeType
    deriving (Eq, Show)

instance TypedNode TypedF where
    nodeType (TypedNegativeF _ n) = n
    nodeType (TypedReferenceF _ n) = n
    nodeType (TypedDereferenceF _ n) = n
    nodeType (TypedSimpleF _ n) = n

data TypedFactor =
    TypedGroupedFactor TypedExpression NodeType |
    TypedFunCallFactor TypedFunCall NodeType |
    TypedReadFactor NodeType |
    TypedDereferenceFactor Identifier NodeType |
    TypedVarFactor Identifier NodeType |
    TypedArrayReferenceFactor Identifier TypedExpression NodeType |
    TypedNumberFactor Int NodeType |
    TypedStringFactor String NodeType
    deriving (Eq, Show)

instance TypedNode TypedFactor where
    nodeType (TypedGroupedFactor _ n) = n
    nodeType (TypedFunCallFactor _ n) = n
    nodeType (TypedReadFactor n) = n
    nodeType (TypedDereferenceFactor _ n) = n
    nodeType (TypedVarFactor _ n) = n
    nodeType (TypedArrayReferenceFactor _ _ n) = n
    nodeType (TypedNumberFactor _ n) = n
    nodeType (TypedStringFactor _ n) = n

data TypedFunCall =
    TypedFunCall Identifier [TypedExpression] NodeType
    deriving (Eq, Show)

instance TypedNode TypedFunCall where
    nodeType (TypedFunCall _ _ n) = n
