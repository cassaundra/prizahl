module Language.Prizahl.Error
  ( Error(..)
  ) where

data Error t
  = TypeMismatch t t
  | ArityMismatch Int Int
  | UnboundVariable String -- TODO use Identifier
  | OtherError String
  deriving Eq

instance Show t => Show (Error t) where
  show (TypeMismatch expected got) =
    "type mismatch: " ++ formatExpected expected got
  show (ArityMismatch expected got) =
    "arity mismatch: " ++ formatExpected expected got
  show (UnboundVariable ident) = "variable not bound: " ++ ident
  show (OtherError s) = "error: " ++ s

formatExpected :: Show a => a -> a -> String
formatExpected expected got =
  "expected " ++ show expected ++ ", got " ++ show got
