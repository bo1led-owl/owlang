{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Control.Applicative
import Data.Foldable
import Data.Text.IO (readFile)
import Frontend.AST
import Frontend.Lex
import Frontend.Parse (parse)
import Options.Applicative hiding (some)
import Text.Megaparsec (errorBundlePretty)
import Prelude hiding (lex, readFile)

data Mode
  = CompileMode
  | PrintAstMode

defaultMode :: Mode
defaultMode = CompileMode

data Args = Args {inputFiles :: [String], mode :: Mode}

printAstMode :: Parser Mode
printAstMode =
  flag' PrintAstMode (long "print-ast" <> help "Print abstract syntax tree")

args :: ParserInfo Args
args =
  info (parseArgs <**> helper) (fullDesc <> header "owl - an owlang compiler")
  where
    parseArgs = Args <$> some (argument str (metavar "FILES")) <*> (pure defaultMode <|> printAstMode)

main :: IO ()
main = do
  (Args inputFiles mode) <- execParser args
  do
    case mode of
      CompileMode -> mapM_ putStrLn inputFiles
      PrintAstMode -> mapM_ printAstMain inputFiles
  where
    printErrors err = mapM_ putStrLn (lines $ errorBundlePretty err)
    printAstMain inputFile = do
      input <- readFile inputFile
      case lex inputFile input of
        Left err -> printErrors err
        Right t -> case parse inputFile t of
          Left err -> printErrors err
          Right ast -> putStr $ showAST ast