module Language.Prizahl.Env
  ( Env
  , defaultEnv
  , bind
  , bindAll
  ) where

import qualified Data.Map                  as M

import           Language.Prizahl.AST
import           Language.Prizahl.Builtins (builtins)

type Env = M.Map Identifier SExpr

defaultEnv :: Env
defaultEnv = builtins

bind :: Identifier -> SExpr -> Env -> Env
bind = M.insert

bindAll :: [(Identifier, SExpr)] -> Env -> Env
bindAll bindings env =
  foldr (uncurry bind) env bindings
