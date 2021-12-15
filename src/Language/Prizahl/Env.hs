module Language.Prizahl.Env where

import qualified Data.Map             as M

import           Language.Prizahl.AST

type Env = M.Map Identifier SExpr

defaultEnv :: M.Map k a
defaultEnv = M.empty

bind :: Identifier -> SExpr -> Env -> Env
bind = M.insert

bindAll :: [(Identifier, SExpr)] -> Env -> Env
bindAll bindings env =
  foldr (uncurry bind) env bindings
