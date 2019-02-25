module While.Interpreter (
    emptyState,

    evalA,
    evalB,
    evalS
  ) where

import Prelude hiding (lookup)

-- This is the module where the environment and state are defined
import While.Env

-- This is the module where our abstract syntax is defined
import While.Syntax

-- | The evaluator for arithmetic expressions
evalA :: Aexp -> State -> Z
evalA (Const z)   _ = z
evalA (Var v)     s = case lookup v s of
                        Nothing -> error $ "Unbound variable: " ++ show v
                        Just z  -> z
evalA (Add a1 a2) s = evalA a1 s + evalA a2 s
evalA (Sub a1 a2) s = evalA a1 s - evalA a2 s
evalA (Mul a1 a2) s = evalA a1 s * evalA a2 s

-- | The evaluator for boolean expressions
evalB :: Bexp -> State -> Bool
evalB BTrue       _ = True
evalB BFalse      _ = False
evalB (Eq a1 a2)  s = evalA a1 s == evalA a2 s
evalB (Le a1 a2)  s = evalA a1 s <= evalA a2 s
evalB (Not b)     s = not (evalB b s)
evalB (And b1 b2) s = evalB b1 s && evalB b2 s

-- | The evaluator for statements
evalS :: Stm -> State -> State
evalS Skip         s = s
evalS _            _ = error "interpreter incomplete"
