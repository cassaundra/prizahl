module Main where

import Language.Prizahl.Prog
import Language.Prizahl.Env
import Language.Prizahl.Builtins
import Language.Prizahl.Eval
import Language.Prizahl.Parser
import Text.Megaparsec
import System.Environment (getArgs)
import Control.Monad.Except (catchError)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.Except (runExcept)
import System.IO (hFlush, stdout)
import System.Console.Haskeline

main :: IO ()
main = do
  args <- getArgs
  case args of
    (fileName:_) -> do
      fileContent <- readFile fileName
      either
        (putStrLn . errorBundlePretty)
        (putStrLn . formatResult . runProgram)
        (runParser file fileName fileContent)
    _ -> runInputT defaultSettings (repl builtins)

repl :: Env -> InputT IO ()
repl env = do
  line <- getInputLine "λ> "
  case line of
    Nothing -> return ()
    Just "" -> repl env
    Just line -> do
      case runParser replLine "repl" line of
        -- add a declaration to the environment
        Right (Left declr) -> repl $ declare declr env

        -- evaluate an expression and recurse
        Right (Right expr) -> do
          let result = runReaderT (eval expr) env
          outputStrLn $ formatResult result
          repl env

        -- parsing errror, print and continue
        Left err -> do
          outputStrLn $ errorBundlePretty err
          repl env

formatResult result = case runExcept result of
  Left err -> "error: " ++ show err
  Right val -> show val
