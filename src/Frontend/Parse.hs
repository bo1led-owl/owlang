{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Frontend.Parse (parse) where

import Control.Applicative
import qualified Control.Monad.State as S
import Data.Either.Combinators
import Data.Functor
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as M
import qualified Data.Set as Set
import Data.Void
import Frontend.AST
import Frontend.Lex
import Frontend.SemAnalysis
import Text.Megaparsec hiding (State, Token, many, parse, some, token)
import qualified Text.Megaparsec as MP
import Types

type Parser s = ParsecT Void TokenStream (S.State s)

token :: (SemanticAnalyzer s) => Token -> Parser s Token
token c = MP.token test (Set.singleton . Tokens . (:| []) . liftToken $ c)
  where
    liftToken :: Token -> WithPos Token
    liftToken = WithPos pos pos 0
      where
        pos = initialPos ""
    test :: WithPos Token -> Maybe Token
    test (WithPos _ _ _ x) = if tokenIs c x then Just x else Nothing

recoverAndRegErr :: (MonadParsec e s m, Default b) => m a -> ParseError s e -> m b
recoverAndRegErr p e = registerParseError e *> (p $> defaultValue)

parens :: (SemanticAnalyzer s) => Parser s a -> Parser s a
parens = between (token LParen) (token RParen)

braces :: (SemanticAnalyzer s) => Parser s a -> Parser s a
braces = between (token LBrace) (token RBrace)

-- angles    = between (symbol "<") (symbol ">")
-- brackets  = between (symbol "[") (symbol "]")

identifier :: (SemanticAnalyzer s) => Parser s String
identifier = do
  ident <- token (Identifier "")
  return $ strFromIdent ident
  where
    strFromIdent :: Token -> String
    strFromIdent (Identifier s) = s
    strFromIdent _ = error "expected identifier"

number :: (SemanticAnalyzer s) => Parser s Integer
number = do
  num <- token (Number 0)
  return $ intFromNumber num
  where
    intFromNumber :: Token -> Integer
    intFromNumber (Number n) = n
    intFromNumber _ = error "expected number"

stmt :: (SemanticAnalyzer s) => Parser s Stmt
stmt =
  choice
    [ uncurry DeclStmt <$> try varDecl,
      RetStmt
        <$> try
          ( token KwReturn
              *> (try (lookAhead (try $ token Semicolon $> Nothing)) <|> Just <$> expr)
          ),
      ExprStmt <$> expr
    ]
    <?> "statement"

handleSemanticAnalyzerError ::
  (SemanticAnalyzer s, Default a) =>
  Int ->
  Either Error a ->
  Parser s a
handleSemanticAnalyzerError off res =
  case res of
    Left err -> do
      registerParseError (FancyError off (Set.singleton (ErrorFail err)))
      return defaultValue
    Right v -> return v

term :: (SemanticAnalyzer s) => Parser s Expr
term =
  choice
    [ (try . parens)
        ( withRecovery
            ( recoverAndRegErr
                (skipManyTill anySingle ((lookAhead . try) (token RParen)))
            )
            expr
        ),
      try call,
      try varRef,
      try unaryExpr,
      numLitExpr
    ]
    <?> "expression"
  where
    varRef = do
      off <- getOffset
      name <- identifier
      analyzer <- S.get
      handleSemanticAnalyzerError off (makeVarRefExpr analyzer name)
    call = do
      off <- getOffset
      name <- identifier
      args <- parens (expr `sepEndBy` token Comma)
      analyzer <- S.get
      handleSemanticAnalyzerError off (makeCallExpr analyzer name args)
    unaryExpr = do
      UnaryExpr
        <$> choice [try $ token Minus $> UnaryMinus]
        <*> term
    numLitExpr = NumLitExpr <$> number

binExpr :: (SemanticAnalyzer s) => Precedence -> Expr -> Parser s Expr
binExpr opPrec lhs = do
  curPrec <- getCurPrec
  if curPrec < opPrec
    then return lhs
    else do
      op <- binOp
      rhs <- term
      nextPrec <- getCurPrec
      rhs <-
        if curPrec < nextPrec
          then do
            binExpr (curPrec + 1) rhs
          else return rhs
      e <- handleBinExpr op lhs rhs
      binExpr opPrec e
  where
    opTable =
      [ (Minus, BinMinus),
        (Plus, BinPlus),
        (Mul, BinMul),
        (Assign, BinAssign)
      ]
    getCurPrec = option (-1) (lookAhead (try (precedence <$> binOp)))
    binOp =
      choice $ case unsnoc opTable of
        Just (h, (tok, op)) -> map (\(t, op) -> try $ token t $> op) h ++ [token tok $> op]
        Nothing -> []
    handleBinExpr op lhs rhs = do
      analyzer <- S.get
      case makeBinaryExpr analyzer op lhs rhs of
        (Left err) -> fail err
        (Right e) -> return e

expr :: (SemanticAnalyzer s) => Parser s Expr
expr = do
  lhs <- term
  binExpr 0 lhs

block :: (SemanticAnalyzer s) => Parser s Block
block = braces $ do
  many (token Semicolon)
  stmtWithRec `endBy` some (token Semicolon $> ())
  where
    stmtWithRec =
      withRecovery
        ( \err -> do
            v <- observing (lookAhead (try (token Semicolon) <|> try (token RBrace)))
            case v of
              Left _ ->
                recoverAndRegErr
                  (skipManyTill anySingle (lookAhead (try (token Semicolon) <|> try (token RBrace))))
                  err
              Right _ -> fail ""
        )
        stmt

-- someSemiWithRec =
--   withRecovery
--     (recoverAndRegErr (skipManyTill anySingle ((lookAhead . try) $ token RBrace) $> ()))
--     (some $ token Semicolon)

fnDecl :: (SemanticAnalyzer s) => Parser s (Name, Decl)
fnDecl = do
  token KwFn
  off <- getOffset
  name <- withRecovery handleNameErr (identifier <?> "function name")
  prevAnalyzer <- S.get
  args <-
    withRecovery
      (recoverAndRegErr (skipManyTill anySingle (lookAhead $ try $ token LBrace)))
      (parens (withRecovery handleArgErr (try (fnArg `sepEndBy` token Comma))))
  (S.lift . S.modify)
    ( modifyAST
        ( flip
            (foldl' (\ast (FnArg n t) -> M.insert n (VarDecl t defaultValue) ast))
            args
        )
    )

  rettype <-
    withRecovery
      (recoverAndRegErr (skipManyTill anySingle (lookAhead $ try $ token LBrace)))
      (identifier <?> "return type")
  body <- block
  analyzer <- S.get
  d <- handleSemanticAnalyzerError off (makeFnDecl analyzer args rettype body)
  S.lift $ S.put $ addDecl' name d prevAnalyzer
  return (name, FnDecl args rettype body)
  where
    fnArg = FnArg <$> (identifier <* token Colon) <*> (identifier <?> "type")
    handleNameErr = recoverAndRegErr (skipManyTill anySingle ((lookAhead . try . token) LParen))
    handleArgErr = recoverAndRegErr (skipManyTill anySingle (lookAhead ((try . token) Comma) <|> (try . token) RParen))

-- optionMaybe :: (SemanticAnalyzer s) => Parser s a -> Parser s (Maybe a)
-- optionMaybe p = option Nothing (Just <$> p)

varDecl :: (SemanticAnalyzer s) => Parser s (Name, Decl)
varDecl = do
  token KwLet
  off <- getOffset
  name <- identifier
  t <- option "" (try (token Colon *> identifier))
  v <- token Assign *> (expr <?> "initializer")
  analyzer <- S.get
  d <- handleSemanticAnalyzerError off (makeVarDecl analyzer t v)
  S.lift $ S.put $ addDecl' name d analyzer
  return (name, d)

varDeclTopLevel :: (SemanticAnalyzer s) => Parser s (Name, Decl)
varDeclTopLevel = do
  prevAnalyzer <- S.get
  token KwLet
  off <- getOffset
  name <- identifier
  t <- option "" (try (token Colon *> identifier))
  v <- token Assign *> (expr <?> "initializer")
  analyzer <- S.get
  d <- handleSemanticAnalyzerError off (makeVarDecl analyzer t v)
  case addDecl name d prevAnalyzer of
    Left err -> do
      registerParseError (FancyError off (Set.singleton (ErrorFail err)))
      return (name, defaultValue)
    Right newAnalyzer -> do
      S.lift $ S.put newAnalyzer
      return (name, d)

topLevelDecl :: (SemanticAnalyzer s) => Parser s (Name, Decl)
topLevelDecl = many (token Semicolon) *> (try fnDecl <|> (varDeclTopLevel <* token Semicolon))

decls :: (SemanticAnalyzer s) => Parser s ()
decls = token StartToken *> many topLevelDecl <* eof $> ()

genericParse :: (SemanticAnalyzer s) => s -> String -> TokenStream -> Either (ParseErrorBundle TokenStream Void) AST
genericParse analyzer filename toks =
  let (res, st) = S.runState (runParserT decls filename toks) analyzer
   in mapRight (const $ getAST st) res

parse :: String -> TokenStream -> Either (ParseErrorBundle TokenStream Void) AST
parse filename toks = do
  ast <- genericParse (DummySemAnalyzer M.empty) filename toks
  genericParse (ActualSemAnalyzer ast) filename toks
