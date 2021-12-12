module Language.Prizahl.Expr where

import           Data.List.NonEmpty (NonEmpty)
import qualified Math.NumberTheory.Primes as P
import Data.Foldable (toList)

type Identifier = String

newtype Factor = Factor (P.Prime Int, Word)
  deriving Eq

instance Show Factor where
  show (Factor (p, 1)) = show $ P.unPrime p
  show (Factor (p, e)) = show (P.unPrime p) ++ "^" ++ show e

-- TODO let

data Expr
  = Variable Identifier
  | Literal Value
  | If Expr Expr Expr
  | Lambda (NonEmpty Identifier) Expr
  | Application Expr [Expr]
  deriving Eq

instance Show Expr where
  show (Variable ident) = ident
  show (Literal value) = show value
  show (If test a b) = "(if " ++ show test ++ show a ++ show b ++ ")"
  show (Lambda identifiers body) = "(lambda (" ++ showSpacedList identifiers ++ ") " ++ show body ++ ")"
  show (Application fn args) = "(" ++ show fn ++ " " ++ showSpacedList args ++ ")"

data Value
  = Prime (P.Prime Int)
  | Factorization (NonEmpty Factor)
  | Boolean Bool
  | Symbol String
  | List (NonEmpty Value)
  deriving Eq

instance Show Value where
  show (Prime n)               = show $ P.unPrime n
  show (Factorization factors) = "[" ++ showSpacedList factors ++ "]"
  show (Boolean True)          = "#t"
  show (Boolean False)         = "#f"
  show (Symbol s)              = "'" ++ s
  show (List list)             = "'(" ++ showSpacedList list ++ ")"

showSpacedList :: (Foldable t, Show a) => t a -> String
showSpacedList = unwords . map show . toList
