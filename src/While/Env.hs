module While.Env (
    State,
    emptyState,

    lookup,
    extend
  ) where

import Prelude hiding (lookup)

import While.Syntax

-- | In lecture, we represented the state as a function. Here we represent state
-- as an association list instead.
type State = [(Var,Z)]

-- | The empty state contains no bindings for any variables---it as an empty
-- association list.
emptyState :: State
emptyState = []

-- | 'lookup' finds a variable in the current state. Note that it is a partial
-- function---we don't gracefully handle use of an undefined variable.
lookup :: Var -> State -> Maybe Z
lookup _ []                      = Nothing
lookup v ((v',n):vs) | v' == v   = Just n
                     | otherwise = lookup v vs

-- | 'extend' adds or replaces a binding in the current state. To avoid an
-- exploding association list, we prune any previous binding for @v@.
extend :: State -> Var -> Z -> State
extend vs v n = (v,n) : filter (\(v', _) -> v' /= v) vs
