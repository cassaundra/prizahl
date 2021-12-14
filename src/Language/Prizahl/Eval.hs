module Language.Prizahl.Eval where

import Control.Monad.Except
    ( Except, throwError, runExcept, runExceptT )
import           Control.Monad.Trans.Except (except)
import           Control.Monad.Trans.Reader
import qualified Data.Map                   as M
import           Language.Prizahl.Env
import           Language.Prizahl.Error
import           Language.Prizahl.Prog
import qualified Language.Prizahl.Type      as Type

-- Program

runProgram :: Body -> Except (Error Type.Type) Value
runProgram body = runReaderT (exec body) M.empty

-- Declaration execution

exec :: Body -> Eval Value
exec (Body declrs expr) = local (\env -> foldr declare env declrs) $ eval expr

declare :: Declaration -> Env -> Env
declare (VariableDefinition ident expr) = M.insert ident expr
declare (FunctionDefinition ident params body) =
  M.insert ident (Value $ Lambda params body)


-- Expression evaluation

type Eval = ReaderT Env (Except (Error Type.Type))

eval :: Expr -> Eval Value

eval (Value v) = return v

eval (Variable ident) = do
  env <- ask
  case M.lookup ident env of
    Just value -> eval value
    Nothing    -> throwError $ VariableNotBound ident

eval (Application f args) = do
  f <- eval f
  args <- mapM eval args
  case f of
    Lambda (SingleFormal param) body ->
      local (bind param (Value $ List args)) (exec body)
    Lambda (MultipleFormals params) body ->
      if length args == length params
        then local (bindAll (zip params (fmap Value args))) (exec body)
        else throwError $ ArityMismatch (length params) (length args)
    Builtin f -> either throwError return $ runExcept (f args)
    _ -> throwError $ TypeMismatch Type.Procedure (typeOf f)
