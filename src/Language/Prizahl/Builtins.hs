{-# LANGUAGE FlexibleContexts #-}
module Language.Prizahl.Builtins where

import qualified Data.Map                 as M
import           Language.Prizahl.Env
import           Language.Prizahl.Error
import           Language.Prizahl.Eval
import           Language.Prizahl.Prog
import qualified Language.Prizahl.Type as T
import qualified Math.NumberTheory.Primes as P
import Control.Monad.Except (Except, MonadError (throwError))
import Control.Monad (when)
import Prelude hiding (readList)

-- ifStatement [Boolean bool, a, b] = Right $ if bool then a else b

ifStatement [condition, a, b] = do
  condition <- readBoolean condition
  return $ if condition then a else b
ifStatement args = throwError $ ArityMismatch 3 (length args)

cons [v, lst] = List . (v :) <$> readList lst
cons args = throwError $ ArityMismatch 2 (length args)

car [lst] = head <$> readNonEmpty lst
car args = throwError $ ArityMismatch 2 (length args)

cdr [lst] = List . tail <$> readNonEmpty lst
cdr args = throwError $ ArityMismatch 2 (length args)

builtins :: Env
builtins =
  M.fromList $
  map
    (fmap toBuiltin)
    [("if", ifStatement), ("cons", cons), ("car", car), ("cdr", cdr)]

toBuiltin :: ([Value] -> Except (Error T.Type) Value) -> Expr
toBuiltin = Value . Builtin

readPrime (Prime p) = return p
readPrime v = throwError $ TypeMismatch T.Prime (typeOf v)

readComposite (Factorization f) = return f
readComposite v = throwError $ TypeMismatch T.Composite (typeOf v)

readBoolean (Boolean b) = return b
readBoolean v = throwError $ TypeMismatch T.Boolean (typeOf v)

readSymbol (Symbol s) = return s
readSymbol v = throwError $ TypeMismatch T.Symbol (typeOf v)

readList (List l) = return l
readList v = throwError $ TypeMismatch T.List (typeOf v)

readNonEmpty (List []) = throwError $ OtherError "list empty"
readNonEmpty (List l) = return l
readNonEmpty v = throwError $ TypeMismatch T.List (typeOf v)

-- TODO for lambda and builtin
