module Language.Prizahl.AST
  ( Identifier
  , Body(..)
  , Declaration(..)
  , Formals(..)
  , Expr(..)
  , Value(..)
  , SExpr(..)
  , Atom(..)
  , Factor(..)
  ) where

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
  = EValue Value
  | EVariable Identifier
  | EApplication Expr [Expr]
  | EBegin Body
  | ELet [(Identifier, Expr)] Body

data Value
  = VPrime (P.Prime Integer)
  | VFactorization (NonEmpty Factor)
  | VBoolean Bool
  | VSymbol String
  | VList [Value]
  | VLambda Formals Body
  -- | VBuiltin ([Value] -> Except (Error T.Type) Value)

-- TODO need a different value type since this one has a lambda with a body, and
-- we want a lambda with a sexpr

data SExpr
  = SAtom Atom
  | SVariable Identifier
  | SApplication SExpr [SExpr]

data Atom = APrime (P.Prime Integer)
          | AComposite (NonEmpty Factor)
          | ABoolean Bool
          | ASymbol String
          | AList [Atom]
          | ALambda Formals SExpr
          | ABuiltin ([Atom] -> Except (Error T.Type) Atom)

instance T.Typed Atom where
  typeOf (APrime _)     = T.Number T.Prime
  typeOf (AComposite _) = T.Number T.Composite
  typeOf (ABoolean _)   = T.Boolean
  typeOf (ASymbol _)    = T.Symbol
  typeOf (AList _)      = T.List
  typeOf (ALambda _ _)  = T.Procedure
  typeOf (ABuiltin _)   = T.Procedure

instance Show Atom where
  show (APrime n)           = show $ P.unPrime n
  show (AComposite factors) = "[" ++ showSpacedList factors ++ "]"
  show (ABoolean True)      = "#t"
  show (ABoolean False)     = "#f"
  show (ASymbol s)          = "'" ++ s
  show (AList list)         = "'(" ++ showSpacedList list ++ ")"
  show (ALambda _ _)        = "#<procedure>"
  show (ABuiltin _)         = "#<procedure>"

newtype Factor = Factor (P.Prime Integer, Word)
  deriving (Eq)

instance Show Factor where
  show (Factor (p, 1)) = show $ P.unPrime p
  show (Factor (p, e)) = show (P.unPrime p) ++ "^" ++ show e

showSpacedList :: (Foldable t, Show a) => t a -> String
showSpacedList = unwords . map show . toList
