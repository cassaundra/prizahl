{-# LANGUAGE FlexibleContexts #-}
module Language.Prizahl.Builtins where

import           Control.Monad.Except     (Except, MonadError (throwError))
import           Control.Newtype.Generics (Newtype (unpack))
import           Data.List.NonEmpty       (fromList, toList)
import qualified Data.Map                 as M
import           GHC.Generics
import           Math.NumberTheory.Primes (Prime (unPrime), factorBack,
                                           factorise, nextPrime)
import           Prelude                  hiding (readList)

import           Language.Prizahl.AST
import           Language.Prizahl.Error
import           Language.Prizahl.Type    (Type (Boolean), isTypeOf)
import qualified Language.Prizahl.Type    as T

arityMismatch expected args = throwError $ ArityMismatch expected (length args)

ifStatement [condition, a, b] = do
  condition <- readBoolean condition
  return $ if condition then a else b
ifStatement args = throwError $ ArityMismatch 3 (length args)

list lst = return $ AList lst

cons [v, lst] = AList . (v :) <$> readList lst
cons args     = arityMismatch 2 args

car [lst] = head <$> readNonEmpty lst
car args  = arityMismatch 2 args

cdr [lst] = AList . tail <$> readNonEmpty lst
cdr args  = arityMismatch 2 args

isEmpty [lst] = ABoolean . null <$> readList lst
isEmpty args  = arityMismatch 1 args

length_ [lst] = numberToValue . fromIntegral . length <$> readList lst
length_ args  = arityMismatch 1 args

not_ [bool] = ABoolean . not <$> readBoolean bool
not_ args   = arityMismatch 1 args

and_ lst = ABoolean . and <$> mapM readBoolean lst

or_ lst = ABoolean . and <$> mapM readBoolean lst

xor_ [a, b] = do
  a <- readBoolean a
  b <- readBoolean b
  return $ ABoolean (a /= b)
xor_ args = arityMismatch 2 args

equal [a, b] = return $ ABoolean (a == b)
equal args   = arityMismatch 2 args

addOne [n]  = numberToValue . (+1) <$> readNumber n
addOne args = arityMismatch 1 args

typecheck expected [value] = return $ ABoolean $ isTypeOf value expected
typecheck _ args           = arityMismatch 1 args

builtins =
  M.fromList $
  map
    (fmap toBuiltin)
    [ ("if", ifStatement)
    , ("list", list)
    , ("cons", cons)
    , ("car", car)
    , ("cdr", cdr)
    , ("empty?", isEmpty)
    , ("length", length_)
    , ("not", not_)
    , ("and", and_)
    , ("or", or_)
    , ("xor", xor_)
    , ("equal?", equal)
    , ("add1", addOne)
    , ("zero?", typecheck (T.Number T.Zero))
    , ("prime?", typecheck (T.Number T.Prime))
    , ("comp?", typecheck (T.Number T.Composite ))
    , ("bool?", typecheck T.Boolean)
    , ("symbol?", typecheck T.Symbol )
    , ("list?", typecheck T.List)
    , ("proc?", typecheck T.Procedure)
    ]

toBuiltin :: ([Atom] -> Except (Error T.Type) Atom) -> SExpr
toBuiltin = SAtom . ABuiltin

readPrime (APrime p) = return p
readPrime v          = throwError $ TypeMismatch (T.Number T.Prime) (T.typeOf v)

readComposite (AComposite f) = return f
readComposite v = throwError $ TypeMismatch (T.Number T.Composite) (T.typeOf v)

readNumber AZero = return 0
readNumber (APrime p) = return $ unPrime p
readNumber (AComposite factors) =
  return $ factorBack $ fmap unpack factors
readNumber v = throwError $ TypeMismatch (T.Number T.AnyNumber) (T.typeOf v)

readBoolean (ABoolean b) = return b
readBoolean v            = throwError $ TypeMismatch T.Boolean (T.typeOf v)

readSymbol (ASymbol s) = return s
readSymbol v           = throwError $ TypeMismatch T.Symbol (T.typeOf v)

readList (AList l) = return l
readList v         = throwError $ TypeMismatch T.List (T.typeOf v)

readNonEmpty (AList []) = throwError $ InvalidArgument "list cannot be empty"
readNonEmpty (AList l)  = return l
readNonEmpty v          = throwError $ TypeMismatch T.List (T.typeOf v)

numberToValue 0 = AZero
numberToValue n =
  case factorise n of
    [(prime, 1)] -> APrime prime
    factors      -> AComposite $ fmap Factor factors

-- TODO for lambda and builtin
