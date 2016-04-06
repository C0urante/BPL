module Environment where

import qualified Data.Map as Map

type Identifier = String

data VarType = IntVar
             | StringVar
               deriving (Eq, Show)
data VarMetaType = RawVar
                 | PointerVar
                 | ArrayVar
                   deriving (Eq, Show)
type Var = (VarType, VarMetaType)

showVar :: Var -> String
showVar (t, m) = metaType m ++ " " ++ rawType t where
    rawType IntVar = "integer"
    rawType StringVar = "string"
    metaType RawVar = "raw"
    metaType PointerVar = "pointer to"
    metaType ArrayVar = "array of"

data FunType = IntFun
             | StringFun
             | VoidFun
               deriving (Eq, Show)
type Fun = (FunType, [Var])

data Type = Variable Var
          | Function Fun
            deriving (Eq, Show)

funTypeToVar :: FunType -> Var
funTypeToVar IntFun = (IntVar, RawVar)
funTypeToVar StringFun = (StringVar, RawVar)
funTypeToVar VoidFun = error "Cannot use return of void function in expression"

typeToVar :: Type -> Var
typeToVar (Variable v) = v
typeToVar (Function (t, _)) = funTypeToVar t

type Environment = Map.Map Identifier Type
emptyEnvironment :: Environment
emptyEnvironment = Map.empty

data Scope = GlobalScope Environment
           | NestedScope Environment Scope
             deriving (Eq, Show)

addToScope :: Scope -> Identifier -> Type -> Scope
addToScope (GlobalScope e)   i t = GlobalScope (Map.insert i t e)
addToScope (NestedScope e s) i t = NestedScope (Map.insert i t e) s

lookup :: Scope -> Identifier -> Type
lookup (GlobalScope e) i = case Map.lookup i e of
    (Just t) -> t
    Nothing -> error $ "Reference to undefined identifier " ++ i
lookup (NestedScope e s) i = case Map.lookup i e of
    (Just t) -> t
    Nothing -> Environment.lookup s i

contains :: Scope -> Identifier -> Bool
contains (GlobalScope e) i   = Map.member i e
contains (NestedScope e s) i = Map.member i e || contains s i
