module Language.Prizahl.Eval
  ( runProgram
  , evalExpression
  , declare
  ) where

import           Control.Monad.Except       (Except, runExcept, throwError)
import           Control.Monad.Trans.Reader
import qualified Data.Map                   as M

import           Data.List                  (sort)
import           Language.Prizahl.AST
import           Language.Prizahl.Env
import           Language.Prizahl.Error
import qualified Language.Prizahl.Type      as Type

runProgram :: Body -> Except (Error Type.Type) Atom
runProgram body = runReaderT (eval $ desugarBody body) defaultEnv

evalExpression :: Expr -> Env -> Except (Error Type.Type) Atom
evalExpression expr = runReaderT (eval $ desugar expr)

declare :: Declaration -> Env -> Env
declare (VariableDefinition ident expr) = M.insert ident (desugar expr)
declare (FunctionDefinition ident params body) =
  M.insert ident (SAtom $ ALambda params (desugarBody body))

-- TODO flatten levels of lambda when possible
desugarBody :: Body -> SExpr
desugarBody (Body (d:ds) expr) =
  SApplication (SAtom $ ALambda (MultipleFormals [ident]) body) [value]
  where
    ident =
      case d of
        VariableDefinition ident _   -> ident
        FunctionDefinition ident _ _ -> ident
    value =
      case d of
        VariableDefinition _ value -> desugar value
        FunctionDefinition _ formals body ->
          SAtom $ ALambda formals (desugarBody body)
    body = desugarBody (Body ds expr)

desugarBody (Body [] expr) = desugar expr

desugar :: Expr -> SExpr
desugar (EValue value)        = SAtom $ desugarValue value
desugar (EVariable ident)     = SVariable ident
desugar (EApplication f args) = SApplication (desugar f) (fmap desugar args)
desugar (EBegin body)         = desugarBody body
desugar (ELet bindings body)  = error "not implemented"

desugarValue :: Value -> Atom
desugarValue VZero = AZero
desugarValue (VPrime p) = APrime p
desugarValue (VFactorization [Factor (p, 1)]) = APrime p
desugarValue (VFactorization factors) = AComposite (sort factors)
desugarValue (VBoolean bool) = ABoolean bool
desugarValue (VSymbol symbol) = ASymbol symbol
desugarValue (VList list) = AList (fmap desugarValue list)
desugarValue (VLambda formals body) = ALambda formals (desugarBody body)

type Eval = ReaderT Env (Except (Error Type.Type))

eval :: SExpr -> Eval Atom
eval (SAtom v) = return v
eval (SVariable ident) = do
  env <- ask
  case M.lookup ident env of
    Just value -> eval value
    Nothing    -> throwError $ UnboundVariable ident
eval (SApplication f args) = do
  f <- eval f
  args <- mapM eval args
  case f of
    ALambda (SingleFormal param) body ->
      local (bind param (SAtom $ AList args)) (eval body)
    ALambda (MultipleFormals params) body ->
      if length args == length params
        then local (bindAll (zip params (fmap SAtom args))) (eval body)
        else throwError $ ArityMismatch (length params) (length args)
    ABuiltin f -> either throwError return $ runExcept (f args)
    _ -> throwError $ TypeMismatch Type.Procedure (Type.typeOf f)
