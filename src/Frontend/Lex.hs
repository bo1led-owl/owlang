{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Frontend.Lex
  ( lex,
    tokenIs,
    Token (..),
    TokenStream (..),
    WithPos (..),
  )
where

import Data.Functor
import qualified Data.List as DL
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Proxy
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec hiding (Token, token, tokens)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Prelude hiding (div, lex)

data Token
  = StartToken
  | KwLet
  | KwReturn
  | KwFn
  | Number Integer
  | Identifier String
  | Semicolon
  | Colon
  | Comma
  | Dot
  | Plus
  | Minus
  | Mul
  | Div
  | Assign
  | LParen
  | RParen
  | LBrace
  | RBrace
  deriving (Eq, Ord, Show)

tokenIs :: Token -> Token -> Bool
tokenIs (Identifier _) (Identifier _) = True
tokenIs (Number _) (Number _) = True
tokenIs a b = a == b

data WithPos a = WithPos
  { startPos :: SourcePos,
    endPos :: SourcePos,
    tokenLength :: Int,
    tokenVal :: a
  }
  deriving (Eq, Ord, Show)

instance Functor WithPos where
  fmap f wp = wp {tokenVal = (f . tokenVal) wp}

data TokenStream = TokenStream
  { tokenStreamInput :: T.Text,
    unTokenStream :: [WithPos Token]
  }

instance Stream TokenStream where
  type Token TokenStream = WithPos Token
  type Tokens TokenStream = [WithPos Token]

  tokenToChunk Proxy x = [x]
  tokensToChunk Proxy xs = xs
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  chunkEmpty Proxy = null
  take1_ (TokenStream _ []) = Nothing
  take1_ (TokenStream str (t : ts)) =
    Just
      ( t,
        TokenStream (T.drop (tokensLength pxy (t :| [])) str) ts
      )
  takeN_ n (TokenStream str s)
    | n <= 0 = Just ([], TokenStream str s)
    | null s = Nothing
    | otherwise =
        let (x, s') = splitAt n s
         in case NE.nonEmpty x of
              Nothing -> Just (x, TokenStream str s')
              Just nex -> Just (x, TokenStream (T.drop (tokensLength pxy nex) str) s')
  takeWhile_ f (TokenStream str s) =
    let (x, s') = DL.span f s
     in case NE.nonEmpty x of
          Nothing -> (x, TokenStream str s')
          Just nex -> (x, TokenStream (T.drop (tokensLength pxy nex) str) s')

instance VisualStream TokenStream where
  showTokens Proxy =
    unwords
      . NE.toList
      . fmap (showToken . tokenVal)
  tokensLength Proxy xs = sum (tokenLength <$> xs)

instance TraversableStream TokenStream where
  reachOffset o PosState {..} =
    ( Just (prefix ++ T.unpack restOfLine),
      PosState
        { pstateInput =
            TokenStream
              { tokenStreamInput = postStr,
                unTokenStream = post
              },
          pstateOffset = max pstateOffset o,
          pstateSourcePos = newSourcePos,
          pstateTabWidth = pstateTabWidth,
          pstateLinePrefix = prefix
        }
    )
    where
      prefix =
        if sameLine
          then pstateLinePrefix ++ preLine
          else preLine
      sameLine = sourceLine newSourcePos == sourceLine pstateSourcePos
      newSourcePos =
        case post of
          [] -> case unTokenStream pstateInput of
            [] -> pstateSourcePos
            xs -> endPos (last xs)
          (x : _) -> startPos x
      (pre, post) = splitAt (o - pstateOffset) (unTokenStream pstateInput)
      (preStr, postStr) = T.splitAt tokensConsumed (tokenStreamInput pstateInput)
      preLine = T.unpack . T.reverse . T.takeWhile (/= '\n') . T.reverse $ preStr
      tokensConsumed =
        case NE.nonEmpty pre of
          Nothing -> 0
          Just nePre -> tokensLength pxy nePre
      restOfLine = T.takeWhile (/= '\n') postStr

pxy :: Proxy TokenStream
pxy = Proxy

showToken :: Token -> String
showToken t = inTicks $ case t of
  StartToken -> "START"
  KwLet -> "let"
  KwReturn -> "return"
  KwFn -> "fn"
  (Number _) -> "number"
  (Identifier _) -> "identifier"
  Semicolon -> ";"
  Colon -> ":"
  Comma -> ","
  Dot -> "."
  Plus -> "+"
  Minus -> "-"
  Mul -> "*"
  Div -> "/"
  Assign -> "="
  LParen -> "("
  RParen -> ")"
  LBrace -> "{"
  RBrace -> "}"
  where
    inTicks s = "`" ++ s ++ "`"

type Parser = Parsec Void T.Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") empty

withPos :: Parser a -> Parser (WithPos a)
withPos p = do
  start <- getSourcePos
  res <- p
  end <- getSourcePos
  return $
    WithPos
      { startPos = start,
        endPos = end,
        tokenLength = 0,
        tokenVal = res
      }

withPos' :: Parser (WithPos a) -> Parser (WithPos a)
withPos' p = do
  start <- getOffset
  res <- p
  end <- getOffset
  return $ res {tokenLength = end - start}

semicolon :: Parser (WithPos Token)
semicolon = symbol ";" Semicolon

comma :: Parser (WithPos Token)
comma = symbol "," Comma

colon :: Parser (WithPos Token)
colon = symbol ":" Colon

dot :: Parser (WithPos Token)
dot = symbol "." Dot

plus :: Parser (WithPos Token)
plus = symbol "+" Plus

minus :: Parser (WithPos Token)
minus = symbol "-" Minus

assign :: Parser (WithPos Token)
assign = symbol "=" Assign

mul :: Parser (WithPos Token)
mul = symbol "*" Mul

div :: Parser (WithPos Token)
div = symbol "/" Div

lParen :: Parser (WithPos Token)
lParen = symbol "(" LParen

rParen :: Parser (WithPos Token)
rParen = symbol ")" RParen

lBrace :: Parser (WithPos Token)
lBrace = symbol "{" LBrace

rBrace :: Parser (WithPos Token)
rBrace = symbol "}" RBrace

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: T.Text -> Token -> Parser (WithPos Token)
symbol s t = withPos' (lexeme (withPos (string s $> t)))

keyword :: T.Text -> Token -> Parser (WithPos Token)
keyword s t = withPos' $ lexeme (withPos ((string s <* notFollowedBy alphaNumChar) $> t))

keywords :: [String]
keywords = ["fn", "let", "return"]

kwFn :: Parser (WithPos Token)
kwFn = keyword "fn" KwFn

kwLet :: Parser (WithPos Token)
kwLet = keyword "let" KwLet

kwReturn :: Parser (WithPos Token)
kwReturn = keyword "return" KwReturn

number :: Parser (WithPos Token)
number = lexeme (withPos (Number <$> L.decimal))

identifier :: Parser (WithPos Token)
identifier =
  withPos' . lexeme . withPos $
    do
      firstChar <- identStart
      rest <- many identChar
      let res = firstChar : rest
      if res `elem` keywords
        then error "parsed keyword"
        else return $ Identifier res
  where
    identStart = try letterChar <|> char '_'
    identChar = try alphaNumChar <|> char '_'

token :: Parser (WithPos Token)
token =
  choice
    [ kwLet,
      kwReturn,
      kwFn,
      number,
      identifier,
      semicolon,
      colon,
      comma,
      dot,
      plus,
      minus,
      mul,
      div,
      assign,
      lParen,
      rParen,
      lBrace,
      rBrace
    ]

startToken :: Parser (WithPos Token)
startToken = symbol "" StartToken

tokens :: Parser [WithPos Token]
tokens = (:) <$> startToken <*> (many token <* eof)

lex :: String -> T.Text -> Either (ParseErrorBundle T.Text Void) TokenStream
lex filename input = TokenStream input <$> runParser tokens filename input
