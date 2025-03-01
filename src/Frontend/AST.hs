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
    printAST,
  )
where

import Data.List
import qualified Data.Map as M
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
  show UnaryMinus = "`-`"

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

instance Show BinOp where
  show BinMul = "`*`"
  show BinPlus = "`+`"
  show BinMinus = "`-`"
  show BinAssign = "`=`"

lf :: String
lf = "\n"

printAST :: AST -> IO ()
printAST = mapM_ putStrLn . lines . showAST

showAST decls = intercalate (lf ++ lf) (map (uncurry (showDecl 0)) (M.toList decls))

showDecl level name (FnDecl args rt block) =
  indent level
    ++ "FnDecl "
    ++ name
    ++ "("
    ++ intercalate ", " (map (\(FnArg n t) -> n ++ ": `" ++ t ++ "`") args)
    ++ ") `"
    ++ rt
    ++ "`"
    ++ if null block then "" else lf ++ showBlock (level + 1) block
showDecl level name (VarDecl t e) =
  indent level
    ++ "VarDecl "
    ++ name
    ++ " `"
    ++ t
    ++ "`"
    ++ lf
    ++ showExpr (level + 1) e

showExpr :: Int -> Expr -> String
showExpr level (VarRefExpr n) = indent level ++ "VarRefExpr " ++ n
showExpr level (CallExpr n es) =
  indent level
    ++ "CallExpr "
    ++ n
    ++ lf
    ++ intercalate lf (map (showExpr (level + 1)) es)
showExpr level (BinExpr op lhs rhs) =
  indent level
    ++ "BinExpr "
    ++ show op
    ++ lf
    ++ showExpr (level + 1) lhs
    ++ lf
    ++ showExpr (level + 1) rhs
showExpr level (UnaryExpr op e) =
  indent level
    ++ "UnaryExpr "
    ++ show op
    ++ lf
    ++ showExpr (level + 1) e
showExpr level (NumLitExpr n) =
  indent level
    ++ "NumLitExpr "
    ++ show n

showStmt level (ExprStmt e) =
  indent level
    ++ "ExprStmt"
    ++ lf
    ++ showExpr (level + 1) e
showStmt level (BlockStmt b) =
  indent level
    ++ "BlockStmt "
    ++ lf
    ++ showBlock (level + 1) b
showStmt level (DeclStmt n d) = showDecl level n d
showStmt level (RetStmt e) =
  indent level
    ++ "RetStmt"
    ++ maybe "" (\e -> lf ++ showExpr (level + 1) e) e

showBlock level block = intercalate lf (map (showStmt level) block)

indent level = replicate (level * 2) ' '
