module Backend.IR where

import Data.Word
import Types

type Label = Name

data Type
  = VoidT
  | BoolT
  | IntT Word8
  | UIntT Word8
  | PtrT Type
  deriving (Show)

data IR = IR
  { functions :: [(Name, Function)],
    globalValues :: [(Name, GlobalValue)]
  }
  deriving (Show)

-- instance Show IR where
--   -- show (IR funcs gvalues) = error "todo"
--   show (IR _ _) = error "todo"

newtype Function = Function [(Label, BB)]
  deriving (Show)

data GlobalValue = GlobalValue
  deriving (Show)

newtype BB = BB [Instr]
  deriving (Show)

newtype Value = Value Name
  deriving (Show)

data CmpCond
  = EqCond
  | NeCond
  | UGtCond
  | UGeCond
  | ULtCond
  | ULeCond
  | SGtCond
  | SGeCond
  | SLtCond
  | SLeCond
  deriving (Show)

type Dest = Value

data Instr
  = AddInstr Dest Type Value Value
  | SubInstr Type Value Value
  | -- | ICmpInstr CmpCond Type Value Value
    -- | BrInstr Value Label Label |
    CallVoidInstr Name [Value]
  | CallInstr Dest Type Name [Value]
  | RetVoidInstr
  | RetInstr Type Value
  deriving (Show)
