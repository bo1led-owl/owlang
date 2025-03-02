{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Frontend.AST
  ( AST,
    Type,
    Default (..),
    Block,
    Decl (..),
    Stmt (..),
    FnArg (..),
    Expr (..),
    UnaryOp (..),
    BinOp (..),
    Precedence,
    precedence,
    showAST,
  )
where

import Data.List
import qualified Data.Map as M
import Data.Tree
import Types

type Type = String

class Default a where
  defaultValue :: a

instance Default [a] where
  defaultValue = []

type Block = [Stmt]

type AST = M.Map Name Decl

data FnArg = FnArg Name Type
  deriving (Show)

instance Default FnArg where
  defaultValue = FnArg defaultValue defaultValue

data Decl
  = FnDecl [FnArg] Type Block
  | VarDecl Type Expr
  deriving (Show)

instance Default Decl where
  defaultValue = FnDecl [] defaultValue defaultValue

data Stmt
  = ExprStmt Expr
  | BlockStmt Block
  | DeclStmt Name Decl
  | RetStmt (Maybe Expr)
  deriving (Show)

instance Default Stmt where
  defaultValue = ExprStmt (defaultValue :: Expr)

data Expr
  = VarRefExpr Name
  | CallExpr Name [Expr]
  | BinExpr BinOp Expr Expr
  | UnaryExpr UnaryOp Expr
  | NumLitExpr Integer
  deriving (Show)

instance Default Expr where
  defaultValue = VarRefExpr defaultValue

data UnaryOp
  = UnaryMinus

instance Show UnaryOp where
  show UnaryMinus = "-"

data BinOp
  = BinPlus
  | BinMinus
  | BinMul
  | BinAssign

type Precedence = Int

precedence :: BinOp -> Precedence
precedence BinMul = 3
precedence BinPlus = 2
precedence BinMinus = 2
precedence BinAssign = 1

inTicks :: String -> String
inTicks s = "`" ++ s ++ "`"

instance Show BinOp where
  show BinMul = "*"
  show BinPlus = "+"
  show BinMinus = "-"
  show BinAssign = "="

class Printable a where
  toNode :: a -> Tree String

showAST :: AST -> String
showAST ast = intercalate "\n" (map (unlines . filter notBars . lines . drawTree . toNode) (M.toList ast))
  where
    notBars = any (\x -> x /= ' ' && x /= '|')

instance Printable (Name, Decl) where
  toNode (n, FnDecl args rt body) =
    Node (n ++ "(" ++ intercalate ", " (map formatArg args) ++ ") " ++ inTicks rt) (map toNode body)
    where
      formatArg (FnArg n t) = n ++ ": " ++ inTicks t
  toNode (n, VarDecl t e) = Node (n ++ " " ++ t) [toNode e]

instance Printable Expr where
  toNode (VarRefExpr n) = Node ("VarRefExpr " ++ n) []
  toNode (CallExpr n args) = Node ("CallExpr " ++ n) (map toNode args)
  toNode (BinExpr op lhs rhs) = Node ("BinExpr " ++ inTicks (show op)) [toNode lhs, toNode rhs]
  toNode (UnaryExpr op e) = Node ("UnaryExpr " ++ inTicks (show op)) [toNode e]
  toNode (NumLitExpr n) = Node ("NumLitExpr " ++ show n) []

instance Printable Stmt where
  toNode (ExprStmt e) = Node "ExprStmt" [toNode e]
  toNode (BlockStmt b) = Node "BlockStmt" (map toNode b)
  toNode (DeclStmt n (VarDecl t e)) = Node ("DeclStmt " ++ n ++ " " ++ inTicks t) [toNode e]
  toNode (DeclStmt _ _) = undefined
  toNode (RetStmt e) = Node "RetStmt" (maybe [] (singleton . toNode) e)
