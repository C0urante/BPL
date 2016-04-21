module Types where

import qualified Data.Map as Map

type LineNumber = Int

data VarMetaType =
    RawVar |
    PointerVar |
    ArrayVar Int
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
    metaType (ArrayVar _) = "array of"

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
        (ArrayVar _) -> ArrayNode

funNodeType :: FunType -> NodeType
funNodeType (r, _) = (raw, RawNode) where
    raw = case r of
        VoidFun -> VoidNode
        IntFun -> IntNode
        StringFun -> StringNode

data AssignmentMetaType =
    RawAssignment |
    DereferenceAssignment |
    ArrayAssignment Expression
    deriving (Eq, Show)

--------------------------------------------------------------------------------

data Program =
    Program [Declaration]
    deriving (Eq, Show)

data Declaration =
    VarDec Identifier VarType |
    FunDec Identifier FunReturnType [(Identifier, VarType)] CompoundStmt
    deriving (Eq, Show)

data CompoundStmt =
    CompoundStmt LocalDecs [Statement]
    deriving (Eq, Show)

data LocalDecs =
    LocalDecs [(Identifier, VarType)]
    deriving (Eq, Show)

data Statement =
    ExpressionStmt Expression |
    EmptyExpressionStmt |
    CompoundStmtStmt CompoundStmt |
    IfStmt Expression Statement |
    IfElseStmt Expression Statement Statement |
    WhileStmt Expression Statement |
    ReturnStmt Expression |
    EmptyReturnStmt |
    WriteStmt Expression |
    WritelnStmt
    deriving (Eq, Show)

data Expression =
    AssignmentExpression Identifier AssignmentMetaType Expression NodeType |
    SimpleExpression CompExp NodeType
    deriving (Eq, Show)

instance TypedNode Expression where
    nodeType (AssignmentExpression _ _ _ n) = n
    nodeType (SimpleExpression _ n) = n

data CompExp =
    CompExp E RelOp E NodeType |
    SimpleExp E NodeType
    deriving (Eq, Show)

data RelOp =
    LessThanOrEqualRelOp |
    LessThanRelOp |
    EqualRelOp |
    NotEqualRelOp |
    GreaterThanRelOp |
    GreaterThanOrEqualRelOp
    deriving (Eq, Show)

instance TypedNode CompExp where
    nodeType (CompExp _ _ _ n) = n
    nodeType (SimpleExp _ n) = n

data E =
    AddE E AddOp T NodeType |
    SimpleE T NodeType
    deriving (Eq, Show)

data AddOp =
    PlusAddOp |
    MinusAddOp
    deriving (Eq, Show)

instance TypedNode E where
    nodeType (AddE _ _ _ n) = n
    nodeType (SimpleE _ n) = n

data T =
    MulT T MulOp F NodeType |
    SimpleT F NodeType
    deriving (Eq, Show)

data MulOp =
    TimesMulOp |
    DivMulOp |
    ModMulOp
    deriving (Eq, Show)

instance TypedNode T where
    nodeType (MulT _ _ _ n) = n
    nodeType (SimpleT _ n) = n

data F =
    NegativeF F NodeType |
    ReferenceF Factor NodeType |
    DereferenceF Factor NodeType |
    SimpleF Factor NodeType
    deriving (Eq, Show)

instance TypedNode F where
    nodeType (NegativeF _ n) = n
    nodeType (ReferenceF _ n) = n
    nodeType (DereferenceF _ n) = n
    nodeType (SimpleF _ n) = n

data Factor =
    GroupedFactor Expression NodeType |
    FunCallFactor FunCall NodeType |
    ReadFactor NodeType |
    DereferenceFactor Identifier NodeType |
    VarFactor Identifier NodeType |
    ArrayReferenceFactor Identifier Expression NodeType |
    NumberFactor Int NodeType |
    StringFactor String NodeType
    deriving (Eq, Show)

instance TypedNode Factor where
    nodeType (GroupedFactor _ n) = n
    nodeType (FunCallFactor _ n) = n
    nodeType (ReadFactor n) = n
    nodeType (DereferenceFactor _ n) = n
    nodeType (VarFactor _ n) = n
    nodeType (ArrayReferenceFactor _ _ n) = n
    nodeType (NumberFactor _ n) = n
    nodeType (StringFactor _ n) = n

data FunCall =
    FunCall Identifier [Expression] NodeType
    deriving (Eq, Show)

instance TypedNode FunCall where
    nodeType (FunCall _ _ n) = n
