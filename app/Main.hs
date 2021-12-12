module Main where

import Language.Prizahl.AST
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
        (print . runProgram)
        (runParser body fileName fileContent)
    _ -> runInputT defaultSettings (repl defaultEnv)

repl :: Env -> InputT IO ()
repl env = do
  line <- getInputLine "λ> "
  case line of
    Nothing -> return ()
    Just line -> do
      case (runParser replLine "repl" line) of
        -- add a declaration to the environment
        Right (ReplDeclr declr) -> repl $ declare declr env

        -- evaluate an expression and recurse
        Right (ReplExpr expr) -> do
          let result = (runReaderT (eval expr) env)
          case (runExcept result) of
            Left err -> outputStrLn $ "; " ++ err
            Right val -> outputStrLn $ show val
          repl env

        -- parsing errror, print and continue
        Left err -> do
          outputStrLn $ errorBundlePretty err
          repl env
