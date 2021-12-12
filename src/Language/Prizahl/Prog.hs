module Language.Prizahl.Prog where

import           Data.Foldable            (toList)
import           Data.List.NonEmpty       (NonEmpty)
import           Language.Prizahl.Error
import qualified Math.NumberTheory.Primes as P

data ReplLine = ReplExpr Expr | ReplDeclr Declaration
  deriving Show

data Body = Body [Declaration] Expr

instance Show Body where
  show (Body [] expr) = show expr
  show (Body declarations expr) =
    showSpacedList declarations ++ " " ++ show expr

data Declaration
  = VariableDefinition Identifier Expr
  | FunctionDefinition Identifier Formals Body

instance Show Declaration where
  show (VariableDefinition ident expr) =
    "(define " ++ ident ++ " " ++ show expr ++ ")"
  show (FunctionDefinition ident args body) =
    "(define (" ++
    ident ++ " " ++ show args ++ ") " ++ show body ++ ")"

newtype Factor = Factor (P.Prime Integer, Word)
  deriving (Eq)

instance Show Factor where
  show (Factor (p, 1)) = show $ P.unPrime p
  show (Factor (p, e)) = show (P.unPrime p) ++ "^" ++ show e

type Identifier = String

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
  | If Expr Expr Expr
  | Application Expr [Expr]
  -- | Let [(Identifier, Expr)] Body
  -- | Begin Body

instance Show Expr where
  show (Value value) = show value
  show (Variable ident) = ident
  show (If test a b) = "(if " ++ showSpacedList [test, a, b] ++ ")"
  show (Application fn args) =
    "(" ++ show fn ++ " " ++ showSpacedList args ++ ")"
  -- show (Let bindings body) = "TODO"
  -- show (Begin body) = "TODO"

data Value
  = Prime (P.Prime Integer)
  | Factorization (NonEmpty Factor)
  | Boolean Bool
  | Symbol String
  | List [Value]
  | Lambda Formals Body
  | Builtin ([Value] -> Either Error Value)

instance Show Value where
  show (Prime n) = show $ P.unPrime n
  show (Factorization factors) = "[" ++ showSpacedList factors ++ "]"
  show (Boolean True) = "#t"
  show (Boolean False) = "#f"
  show (Symbol s) = "'" ++ s
  show (List list) = "'(" ++ showSpacedList list ++ ")"
  show (Lambda formals body) =
    "(lambda " ++ show formals ++ " " ++ show body ++ ")"
  show (Builtin _) = "#<built-in procedure>"

showSpacedList :: (Foldable t, Show a) => t a -> String
showSpacedList = unwords . map show . toList

isNumber (Prime _)         = True
isNumber (Factorization _) = True
isNumber _                 = False

isPrime (Prime _) = True
isPrime _         = False

isComposite (Factorization _) = True
isComposite _                 = False

isBoolean (Boolean _) = True
isBoolean _           = False

isSymbol (Symbol _) = True
isSymbol _          = False

isList (List _) = True
isList _        = False

isLambda (Lambda _ _) = True
isLambda _            = False
