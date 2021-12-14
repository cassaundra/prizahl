{-# LANGUAGE FlexibleContexts #-}
module Language.Prizahl.Builtins where

import           Control.Monad            (when)
import           Control.Monad.Except     (Except, MonadError (throwError))
import qualified Data.Map                 as M
import qualified Math.NumberTheory.Primes as P
import           Prelude                  hiding (readList)

import           Language.Prizahl.AST
import           Language.Prizahl.Env
import           Language.Prizahl.Error
import           Language.Prizahl.Eval
import qualified Language.Prizahl.Type    as T

ifStatement [condition, a, b] = do
  condition <- readBoolean condition
  return $ if condition then a else b
ifStatement args = throwError $ ArityMismatch 3 (length args)

list lst = return $ List lst

cons [v, lst] = List . (v :) <$> readList lst
cons args     = throwError $ ArityMismatch 2 (length args)

car [lst] = head <$> readNonEmpty lst
car args  = throwError $ ArityMismatch 2 (length args)

cdr [lst] = List . tail <$> readNonEmpty lst
cdr args  = throwError $ ArityMismatch 2 (length args)

builtins :: Env
builtins =
  M.fromList $
  map
    (fmap toBuiltin)
    [ ("if", ifStatement)
    , ("list", list)
    , ("cons", cons)
    , ("car", car)
    , ("cdr", cdr)
    ]

toBuiltin :: ([Value] -> Except (Error T.Type) Value) -> Expr
toBuiltin = Value . Builtin

-- TODO readNumber

readPrime (Prime p) = return p
readPrime v         = throwError $ TypeMismatch (T.Number T.Prime) (T.typeOf v)

readComposite (Factorization f) = return f
readComposite v = throwError $ TypeMismatch (T.Number T.Composite) (T.typeOf v)

readBoolean (Boolean b) = return b
readBoolean v           = throwError $ TypeMismatch T.Boolean (T.typeOf v)

readSymbol (Symbol s) = return s
readSymbol v          = throwError $ TypeMismatch T.Symbol (T.typeOf v)

readList (List l) = return l
readList v        = throwError $ TypeMismatch T.List (T.typeOf v)

readNonEmpty (List []) = throwError $ OtherError "list empty"
readNonEmpty (List l)  = return l
readNonEmpty v         = throwError $ TypeMismatch T.List (T.typeOf v)


-- TODO for lambda and builtin
