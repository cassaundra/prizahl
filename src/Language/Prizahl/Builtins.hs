module Language.Prizahl.Builtins where

import qualified Data.Map                 as M
import           Language.Prizahl.Env
import           Language.Prizahl.Error
import           Language.Prizahl.Eval
import           Language.Prizahl.Prog
import           Language.Prizahl.Type    (Type)
import qualified Math.NumberTheory.Primes as P

cons [v, List lst] = Right $ List (v:lst)
cons _             = Left $ OtherError "invalid"

car [List []]    = Left $ OtherError "empty list"
car [List (x:_)] = Right $ x
car _            = Left $ OtherError "invalid"

cdr [List []]     = Left $ OtherError "empty list"
cdr [List (_:xs)] = Right $ List xs
cdr _             = Left $ OtherError "invalid"

builtins :: Env
builtins =
  M.fromList $
  map
    (fmap toBuiltin)
    [("cons", cons), ("car", car), ("cdr", cdr)]

toBuiltin :: ([Value] -> Either (Error Type) Value) -> Expr
toBuiltin = Value . Builtin
