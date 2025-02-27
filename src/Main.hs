module Main where

import Data.Text.IO (readFile)
import Frontend.AST
import Frontend.Lex
import Frontend.Parse
import Text.Megaparsec (errorBundlePretty)
import Prelude hiding (lex, readFile)

inputFile :: String
inputFile = "input.txt"

main :: IO ()
main = do
  input <- readFile inputFile
  case lex inputFile input of
    Left err -> mapM_ putStrLn (lines $ errorBundlePretty err)
    Right t -> case parse inputFile t of
      Left err -> mapM_ putStrLn (lines $ errorBundlePretty err)
      Right ast -> printAST ast