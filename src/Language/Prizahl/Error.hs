module Language.Prizahl.Error
  ( Error(..)
  ) where

data Error t
  = TypeMismatch t t
  | ArityMismatch Int Int
  | VariableNotBound String -- TODO use Identifier
  | OtherError String
  deriving Eq

instance Show t => Show (Error t) where
  show (TypeMismatch expected got)  = formatExpected expected got
  show (ArityMismatch expected got) = formatExpected expected got
  show (VariableNotBound ident)     = "variable not bound: " ++ ident
  show (OtherError s)               = s

formatExpected :: Show a => a -> a -> String
formatExpected expected got =
  "expected " ++ show expected ++ ", got " ++ show got
