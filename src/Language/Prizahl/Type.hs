module Language.Prizahl.Type where

import           Text.Megaparsec.Char (lowerChar)

data Type = Prime | Composite | Boolean | Symbol | List | Procedure
  deriving (Show, Eq)
