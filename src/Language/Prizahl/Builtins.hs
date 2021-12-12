module Language.Prizahl.Builtins where

import qualified Data.Map                 as M
import           Language.Prizahl.Env
import           Language.Prizahl.Error
import           Language.Prizahl.Eval
import           Language.Prizahl.Prog
import qualified Math.NumberTheory.Primes as P

cons [v, List lst] = Right $ List (v:lst)
cons _ = Left "invalid"

car [List (x:_)] = Right $ x
car _ = Left "invalid"

cdr [List (_:xs)] = Right $ List xs
cdr _ = Left "invalid"

builtins :: Env
builtins =
  M.fromList $ map (fmap toBuiltin) [("cons", cons), ("car", car), ("cdr", cdr)]

toBuiltin :: ([Value] -> Either Error Value) -> Expr
toBuiltin = Value . Builtin
