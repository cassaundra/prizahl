module Language.Prizahl.Error
  ( Error(..)
  ) where

data Error t
  = TypeMismatch t t
  | ArityMismatch Int Int
  | UnboundVariable String -- TODO use Identifier
  | InvalidArgument String
  deriving Eq

instance Show t => Show (Error t) where
  show (TypeMismatch expected got) =
    "type mismatch: " ++ formatMismatch expected got
  show (ArityMismatch expected got) =
    "arity mismatch: " ++ formatMismatch expected got
  show (UnboundVariable ident) = "variable not bound: " ++ ident
  show (InvalidArgument s) = "invalid argument: " ++ s

formatMismatch :: Show a => a -> a -> String
formatMismatch expected got =
  "expected " ++ show expected ++ ", got " ++ show got
