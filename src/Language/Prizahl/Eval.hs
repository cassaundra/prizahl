module Language.Prizahl.Eval where

import Control.Monad.Except (Except, throwError)
import Control.Monad.Trans.Reader
import qualified Data.Map as M
import Language.Prizahl.AST

type Env = M.Map Identifier Expr

defaultEnv :: M.Map k a
defaultEnv = M.empty

runProgram :: Body -> Except String Value
runProgram body =
  runReaderT (exec body) M.empty

exec :: Body -> Eval Value
exec (Body declrs expr) = do
  local (\env -> foldr declare env declrs) $ eval expr

declare :: Declaration -> Env -> Env
declare (VariableDefinition ident expr) = M.insert ident expr
declare (FunctionDefinition ident params body) =
  M.insert ident (Value $ Lambda params body)

type Eval = ReaderT Env (Except String)

eval :: Expr -> Eval Value

eval (Value v) = return v

eval (Variable ident) = do
  env <- ask
  case M.lookup ident env of
    Just value -> eval value
    Nothing -> throwError $ "variable not bound: " ++ ident

eval (If test a b) = do
  test <- eval test
  case test of
    Boolean True -> eval a
    Boolean False -> eval b
    _ -> throwError "expression in if conditional did not evaluate to a boolean"

eval (Application f args) = do
  f <- eval f
  args <- mapM eval args
  case f of
    Lambda (SingleFormal param) body ->
      local (bind param (Value $ List args)) (exec body)
    Lambda (MultipleFormals params) body ->
      if length args == length params
        then local (bindAll (zip params (fmap Value args))) (exec body)
        else throwError
               "number of parameters does not match number of arguments"
    _ -> throwError "first argument did not evaluate to a procedure"

bind :: Identifier -> Expr -> Env -> Env
bind = M.insert

bindAll :: [(Identifier, Expr)] -> Env -> Env
bindAll bindings env =
  foldr (uncurry bind) env bindings
