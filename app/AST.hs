module AST where

import Data.Map (Map)

data Op
  = Add
  | Subtract
  | Multiply
  | Divide
  | And
  | Or
  | Eq
  | Neq
  | Lt
  | Gt
  | Lte
  | Gte
  deriving (Show, Eq)

data UOp = Not
  deriving (Show, Eq)

data Value
  = VNil
  | VBool Bool
  | VNumber Double
  | VString String
  deriving (Show, Eq)

data Expr
  = EValue Value
  | EVar String
  | EBinOp Op Expr Expr
  | EUnOp UOp Expr
  deriving (Show, Eq)

data Statement
  = SAssign String Expr
  | SIf Expr Block (Maybe Block)
  | SExpr Expr
  deriving (Show, Eq)

type Block = [Statement]

type Environment = Map String Value