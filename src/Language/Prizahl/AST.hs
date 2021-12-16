{-# LANGUAGE DeriveGeneric #-}

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

import           Control.Monad.Except       (Except)
import           Control.Monad.Reader       (Reader, local)
import           Control.Newtype.Generics   (Newtype (unpack))
import           Data.Foldable              (toList)
import           Data.Function              (on)
import           GHC.Generics               (Generic)
import qualified Math.NumberTheory.Primes   as P

import           Control.Monad.Trans.Reader (ask, runReader)
import           Language.Prizahl.Error
import qualified Language.Prizahl.Type      as T

type Identifier = String

data Body = Body [Declaration] Expr

data Declaration
  = VariableDefinition Identifier Expr
  | FunctionDefinition Identifier Formals Body

data Formals
  = SingleFormal Identifier
  | MultipleFormals [Identifier]
  deriving (Eq)

data Expr
  = EValue Value
  | EVariable Identifier
  | EApplication Expr [Expr]
  | EBegin Body
  | ELet [(Identifier, Expr)] Body

data Value
  = VZero
  | VPrime (P.Prime Integer)
  | VFactorization [Factor]
  | VBoolean Bool
  | VSymbol String
  | VList [Value]
  | VLambda Formals Body

data SExpr
  = SAtom Atom
  | SVariable Identifier
  | SApplication SExpr [SExpr]
  deriving (Eq)

data Atom = AZero
          | APrime (P.Prime Integer)
          | AComposite [Factor]
          | ABoolean Bool
          | ASymbol String
          | AList [Atom]
          | ALambda Formals SExpr
          | ABuiltin ([Atom] -> Except (Error T.Type) Atom)

instance Eq Atom where
  (==) (APrime a) (APrime b)         = a == b
  (==) (AComposite a) (AComposite b) = a == b
  (==) (ABoolean a) (ABoolean b)     = a == b
  (==) (ASymbol a) (ASymbol b)       = a == b
  (==) (AList a) (AList b)           = a == b
  (==) (ALambda af a) (ALambda bf b) = af == bf && a == b
  (==) _ _                           = False

-- TODO lambda eq doesn't really work for non-trivial lambdas
-- builtins don't really work as-is, but would if they were referenced by name

instance T.Typed Atom where
  typeOf AZero          = T.Number T.Zero
  typeOf (APrime _)     = T.Number T.Prime
  typeOf (AComposite _) = T.Number T.Composite
  typeOf (ABoolean _)   = T.Boolean
  typeOf (ASymbol _)    = T.Symbol
  typeOf (AList _)      = T.List
  typeOf (ALambda _ _)  = T.Procedure
  typeOf (ABuiltin _)   = T.Procedure

instance Show Atom where
  show atom = runReader (showInner atom) False

showInner :: Atom -> Reader Bool String
showInner AZero                = return "0"
showInner (APrime n)           = return $ show $ P.unPrime n
showInner (AComposite factors) = return $ "[" ++ showSpacedList factors ++ "]"
showInner (ABoolean True)      = return "#t"
showInner (ABoolean False)     = return "#f"
showInner (ALambda _ _)        = return "#<procedure>"
showInner (ABuiltin _)         = return "#<procedure>"
showInner (ASymbol s) = possiblyQuote s
showInner (AList list) = do
  inner <- local (const True) (unwords <$> mapM showInner list)
  possiblyQuote $ "(" ++ inner ++ ")"

possiblyQuote :: String -> Reader Bool String
possiblyQuote s = do
  inQuoted <- ask
  return $
    if inQuoted
      then s
      else "'" ++ s

newtype Factor = Factor (P.Prime Integer, Word)
  deriving (Generic, Eq)

instance Newtype Factor

instance Ord Factor where
  compare = compare `on` fst . unpack

instance Show Factor where
  show (Factor (p, 1)) = show $ P.unPrime p
  show (Factor (p, e)) = show (P.unPrime p) ++ "^" ++ show e

showSpacedList :: (Foldable t, Show a) => t a -> String
showSpacedList = unwords . map show . toList
