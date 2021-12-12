module Main where

import Language.Prizahl.AST
import Language.Prizahl.Eval
import Language.Prizahl.Parser
import Text.Megaparsec
import System.Environment (getArgs)

main :: IO ()
main = do
  fileName <- head <$> getArgs
  fileContent <- readFile fileName
  either
    (putStrLn . errorBundlePretty)
    (print . runProgram)
    (runParser body "repl" fileContent)
