module Language.Prizahl.Type
  ( Type(..)
  , NumberType(..)
  , Typed(..)
  , isTypeOf
  ) where

data Type
  = Number NumberType
  | Boolean
  | Symbol
  | List
  | Procedure
  | Any
  deriving (Eq)

instance Show Type where
  show (Number n) = show n
  show Boolean    = "boolean"
  show Symbol     = "symbol"
  show List       = "list"
  show Procedure  = "procedure"
  show Any        = "any"

data NumberType = Zero | Prime | Composite | AnyNumber
  deriving Eq

instance Show NumberType where
  show Zero      = "zero"
  show Prime     = "prime number"
  show Composite = "composite number"
  show AnyNumber = "number"

class Typed a where
  typeOf :: a -> Type

isTypeOf :: Typed a => a -> Type -> Bool
isTypeOf typedValue type_ =
  case (typeOf typedValue, type_) of
    (_, Any)                     -> True
    (Number _, Number AnyNumber) -> True
    (a, b)                       -> a == b
