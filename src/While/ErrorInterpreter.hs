module While.ErrorInterpreter (
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
evalA :: Aexp -> State -> Maybe Z
evalA (Const x)   _ = Just x
evalA (Var v)     s = lookup v s
evalA (Add a1 a2) s = case evalA a1 s of
                        Nothing -> Nothing
                        Just z1 -> case evalA a2 s of
                                     Nothing -> Nothing
                                     Just z2 -> Just $ z1 + z2
evalA (Sub a1 a2) s = case evalA a1 s of
                        Nothing -> Nothing
                        Just z1 -> case evalA a2 s of
                                     Nothing -> Nothing
                                     Just z2 -> Just $ z1 - z2
evalA (Mul a1 a2) s = case evalA a1 s of
                        Nothing -> Nothing
                        Just z1 -> case evalA a2 s of
                                     Nothing -> Nothing
                                     Just z2 -> Just $ z1 * z2

-- | The evaluator for boolean expressions
evalB :: Bexp -> State -> Maybe Bool
evalB BTrue       _ = Just True
evalB BFalse      _ = Just False
evalB (Eq a1 a2)  s = case evalA a1 s of
                        Nothing -> Nothing
                        Just z1 -> case evalA a2 s of
                                     Nothing -> Nothing
                                     Just z2 -> Just $ z1 == z2
evalB (Le a1 a2)  s = case evalA a1 s of
                        Nothing -> Nothing
                        Just z1 -> case evalA a2 s of
                                     Nothing -> Nothing
                                     Just z2 -> Just $ z1 <= z2
evalB (Not b)     s = case evalB b s of
                        Nothing -> Nothing
                        Just f  -> Just $ not f
evalB (And b1 b2) s = case evalB b1 s of
                        Nothing -> Nothing
                        Just f1 -> case evalB b2 s of
                                     Nothing -> Nothing
                                     Just f2 -> Just $ f1 && f2

-- | The evaluator for statements
evalS :: Stm -> State -> Maybe State
evalS Skip         s = Just s
evalS _            _ = error "interpreter incomplete"
