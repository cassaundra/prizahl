module Language.Prizahl.Env where

import Language.Prizahl.Prog
import qualified Data.Map                   as M

type Env = M.Map Identifier Expr

defaultEnv :: M.Map k a
defaultEnv = M.empty

bind :: Identifier -> Expr -> Env -> Env
bind = M.insert

bindAll :: [(Identifier, Expr)] -> Env -> Env
bindAll bindings env =
  foldr (uncurry bind) env bindings
