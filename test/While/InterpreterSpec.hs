module While.InterpreterSpec (
    main,
    spec
  ) where

import Data.List (sort)
import Data.Maybe (fromJust)
import Test.Hspec

import While.Env
import While.Parser
import While.Syntax
import While.Interpreter

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "skip" $
    it "skips (doesn't do anything)" $
    run "skip" `shouldBe` []
  describe "assignment" $
    it "assigns 'a' the value 3" $
    run "a := 3" `shouldBe` [("a", 3)]
  describe "sequencing" $
    it "assigns 'a' the value 3 and 'b' the value 4" $
    run "a := 3; b := 4" `shouldBe` [("a", 3), ("b", 4)]
  describe "if-then-else" $
    it "assigns 'a' the value 3 and then tests it" $
    run "a := 3; if a = 3 then b := 5 else b := 6" `shouldBe` [("a", 3), ("b", 5)]
  describe "while" $
    it "computes the sum of 1 to 10" $
    run "sum := 0; i := 1; while i <= 10 (sum := sum + i; i := i + 1)" `shouldBe` [("i", 11), ("sum", 55)]

-- | Run the interpreter
run :: String -> State
run s = sort $ evalS (parse s) emptyState
  where
    parse :: String -> Stm
    parse = fromJust . parseWhile
