module Main where

import           Control.Monad.Except       (catchError)
import           Control.Monad.Trans.Except (runExcept)
import           Control.Monad.Trans.Reader (runReaderT)
import           System.Console.Haskeline
import           System.Console.Pretty
import           System.Environment         (getArgs)
import           System.IO                  (hFlush, stdout)
import           Text.Megaparsec

import           Language.Prizahl.AST
import           Language.Prizahl.Builtins
import           Language.Prizahl.Env
import           Language.Prizahl.Eval
import           Language.Prizahl.Parser

main :: IO ()
main = do
  args <- getArgs
  case args of
    (fileName:_) -> do
      fileContent <- readFile fileName
      putStrLn $
        either
          (errorStyle . init . errorBundlePretty)
          (formatResult . runProgram)
          (parseFile fileName fileContent)
    _ -> runInputT defaultSettings (repl builtins)

repl :: Env -> InputT IO ()
repl env = do
  line <- getInputLine "λ> "
  case line of
    Nothing -> return ()
    Just "" -> repl env
    Just line -> do
      case parseReplLine line of
        -- add a declaration to the environment
        Right (Left declr) -> repl $ declare declr env

        -- evaluate an expression and recurse
        Right (Right expr) -> do
          let result = runReaderT (eval expr) env
          outputStrLn $ formatResult result
          repl env

        -- parsing errror, print and continue
        Left err -> do
          outputStrLn $ errorStyle . init $ errorBundlePretty err
          repl env

formatResult result = case runExcept result of
  Left err  -> errorStyle $ show err
  Right val -> show val

errorStyle = style Faint . color Red
