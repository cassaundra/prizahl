module Language.Prizahl.Builtins where

import qualified Data.Map                 as M
import           Language.Prizahl.Env
import           Language.Prizahl.Error
import           Language.Prizahl.Eval
import           Language.Prizahl.Prog
import qualified Math.NumberTheory.Primes as P

square :: Value -> Either Error Value
square _ = Left "not implemented"

builtins :: Env
builtins =
  M.fromList $ map (fmap toBuiltin) [("square", square)]

toBuiltin :: (Value -> Either Error Value) -> Expr
toBuiltin = Value . Builtin
