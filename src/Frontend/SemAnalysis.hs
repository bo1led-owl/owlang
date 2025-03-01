{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Frontend.SemAnalysis where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe
import Frontend.AST
import Types

type Error = String

class SemanticAnalyzer a where
  makeUnaryExpr :: a -> UnaryOp -> Expr -> Either Error Expr
  makeBinaryExpr :: a -> BinOp -> Expr -> Expr -> Either Error Expr
  makeVarRefExpr :: a -> Name -> Either Error Expr
  makeCallExpr :: a -> Name -> [Expr] -> Either Error Expr
  makeVarDecl :: a -> Type -> Expr -> Either Error Decl
  makeFnDecl :: a -> [FnArg] -> Type -> Block -> Either Error Decl
  modifyAST :: (AST -> AST) -> a -> a
  getAST :: a -> AST
  addDecl :: Name -> Decl -> a -> Either Error a
  addDecl n d s =
    if n `M.notMember` getAST s
      then Right $ modifyAST (M.insert n d) s
      else Left $ "`" ++ n ++ "` already defined"
  addDecl' :: Name -> Decl -> a -> a
  addDecl' n d = modifyAST (M.insert n d)

newtype DummySemAnalyzer = DummySemAnalyzer AST

instance SemanticAnalyzer DummySemAnalyzer where
  makeUnaryExpr _ op e = Right (UnaryExpr op e)
  makeBinaryExpr _ op l r = Right (BinExpr op l r)
  makeVarRefExpr _ n = Right (VarRefExpr n)
  makeCallExpr _ n args = Right (CallExpr n args)
  makeVarDecl _ t e = Right $ VarDecl t e
  makeFnDecl _ args t b = Right $ FnDecl args t b
  modifyAST f (DummySemAnalyzer ast) = DummySemAnalyzer (f ast)
  getAST (DummySemAnalyzer ast) = ast

newtype ActualSemAnalyzer = ActualSemAnalyzer AST

typeEq :: Type -> Type -> Bool
typeEq lhs rhs = lhs == rhs || null lhs || null rhs

instance SemanticAnalyzer ActualSemAnalyzer where
  modifyAST f (ActualSemAnalyzer ast) = ActualSemAnalyzer (f ast)
  getAST (ActualSemAnalyzer ast) = ast
  makeUnaryExpr _ op e = Right (UnaryExpr op e)
  makeBinaryExpr (ActualSemAnalyzer ast) op lhs rhs =
    let (ltype, rtype) = (getType ast lhs, getType ast rhs)
     in if ltype `typeEq` rtype
          then
            Right $ BinExpr op lhs rhs
          else do
            Left $ "mismatched types `" ++ ltype ++ "` and `" ++ rtype ++ "`"
  makeVarRefExpr (ActualSemAnalyzer ast) n =
    case M.lookup n ast of
      Nothing -> undef
      Just (FnDecl {}) -> undef
      Just (VarDecl {}) -> Right $ VarRefExpr n
    where
      undef = Left $ "undefined variable `" ++ n ++ "`"
  makeCallExpr (ActualSemAnalyzer ast) n args =
    case M.lookup n ast of
      Nothing -> undef
      Just (VarDecl {}) -> undef
      Just (FnDecl params _ _) ->
        checkLengths (length params) (length args) *> Right (CallExpr n args)
    where
      undef = Left $ "undefined function `" ++ n ++ "`"
      checkLengths expected actual
        | actual < expected =
            Left $ "too few arguments passed to function: expected " ++ show expected ++ ", but got " ++ show actual
        | actual > expected =
            Left $ "too many arguments passed to function: expected " ++ show expected ++ ", but got " ++ show actual
        | otherwise = Right ()

  makeVarDecl (ActualSemAnalyzer ast) t e =
    let actualType = getType ast e
     in if t `typeEq` actualType
          then Right $ VarDecl actualType e
          else Left $ "unmatched types: expected `" ++ show t ++ "`, got `" ++ show actualType ++ "`"
  makeFnDecl (ActualSemAnalyzer ast) args t b = do
    if not $ checkArgs args
      then
        Left "multiple definitions of a function parameter"
      else
        getBlockType ast b >>= checkBody
    where
      checkArgs = allDifferent . map (\(FnArg n _) -> n)
      allDifferent :: (Eq a) => [a] -> Bool
      allDifferent [] = True
      allDifferent (x : xs) = x `notElem` xs && allDifferent xs
      checkBody bt =
        if t `typeEq` bt
          then Right $ FnDecl args t b
          else Left $ "unmatched return type: expected `" ++ t ++ "`, got `" ++ bt ++ "`"

getType :: AST -> Expr -> Type
getType _ (NumLitExpr _) = "int"
getType ast (VarRefExpr name) = maybe "" getDeclType (M.lookup name ast)
getType ast (CallExpr name _) = maybe "" getDeclType (M.lookup name ast)
getType ast (BinExpr _ lhs _) = getType ast lhs
getType ast (UnaryExpr _ e) = getType ast e

getDeclType :: Decl -> Type
getDeclType (FnDecl _ t _) = t
getDeclType (VarDecl t _) = t

getBlockType :: AST -> Block -> Either Error Type
getBlockType _ [] = Right "void"
getBlockType ast b =
  let ts = mapMaybe getStmtType b
   in case NE.nonEmpty ts of
        Nothing -> Right "void"
        Just ts ->
          if allEqual ts
            then Right $ NE.head ts
            else Left "different types of return values"
  where
    allEqual :: (Eq a) => NE.NonEmpty a -> Bool
    allEqual (_ :| []) = True
    allEqual (x :| xs) = all (== x) xs
    getStmtType :: Stmt -> Maybe Type
    getStmtType (RetStmt Nothing) = Just "void"
    getStmtType (RetStmt (Just e)) = Just $ getType ast e
    getStmtType _ = Nothing