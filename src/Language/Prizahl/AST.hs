module Language.Prizahl.AST where

import           Control.Monad.Except     (Except)
import           Data.Foldable            (toList)
import           Data.List.NonEmpty       (NonEmpty)
import qualified Math.NumberTheory.Primes as P

import           Language.Prizahl.Error
import qualified Language.Prizahl.Type    as T

type Identifier = String

data Body = Body [Declaration] Expr

data Declaration
  = VariableDefinition Identifier Expr
  | FunctionDefinition Identifier Formals Body

data Formals
  = SingleFormal Identifier
  | MultipleFormals [Identifier] -- TODO (Maybe Identifier)

instance Show Formals where
  show (SingleFormal ident)     = ident
  show (MultipleFormals params) = "(" ++ unwords params ++ ")"
  -- show (MultipleFormals params (Just rest)) =
  --   "(" ++ showSpacedList params ++ " . " ++ rest ++ ")"

data Expr
  = Value Value
  | Variable Identifier
  | Application Expr [Expr]
  -- | Let [(Identifier, Expr)] Body
  -- | Begin Body

data Value
  = Prime (P.Prime Integer)
  | Factorization (NonEmpty Factor)
  | Boolean Bool
  | Symbol String
  | List [Value]
  | Lambda Formals Body
  | Builtin ([Value] -> Except (Error T.Type) Value)

instance Show Value where
  show (Prime n)               = show $ P.unPrime n
  show (Factorization factors) = "[" ++ showSpacedList factors ++ "]"
  show (Boolean True)          = "#t"
  show (Boolean False)         = "#f"
  show (Symbol s)              = "'" ++ s
  show (List list)             = "'(" ++ showSpacedList list ++ ")"
  show (Lambda _ _)            = "#<procedure>"
  show (Builtin _)             = "#<procedure>"

newtype Factor = Factor (P.Prime Integer, Word)
  deriving (Eq)

instance Show Factor where
  show (Factor (p, 1)) = show $ P.unPrime p
  show (Factor (p, e)) = show (P.unPrime p) ++ "^" ++ show e

showSpacedList :: (Foldable t, Show a) => t a -> String
showSpacedList = unwords . map show . toList

typeOf :: Value -> T.Type
typeOf (Prime _)         = T.Prime
typeOf (Factorization _) = T.Composite
typeOf (Boolean _)       = T.Boolean
typeOf (Symbol _)        = T.Symbol
typeOf (List _)          = T.List
typeOf (Lambda _ _)      = T.Procedure
typeOf (Builtin _)       = T.Procedure
