module While.ErrorInterpreterSpec (
    main,
    spec
  ) where

import Data.List (sort)
import Data.Maybe (fromJust)
import Test.Hspec

import While.Env
import While.Parser
import While.Syntax
import While.ErrorInterpreter

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "variable" $
    it "attempt to get value of undefined variable" $
    run "a := b" `shouldBe` Nothing
  describe "skip" $
    it "skips (doesn't do anything)" $
    run "skip" `shouldBe` Just []
  describe "assignment" $
    it "assigns 'a' the value 3" $
    run "a := 3" `shouldBe` Just [("a", 3)]
  describe "assignment" $
    it "assigns 'a' the value 1 then the value 2" $
    run "a := 1; a := 2" `shouldBe` Just [("a", 2)]
  describe "sequencing" $
    it "assigns 'a' the value 3 and 'b' the value 4" $
    run "a := 3; b := 4" `shouldBe` Just [("a", 3), ("b", 4)]
  describe "if-then-else" $
    it "assigns 'a' the value 3 and then tests it" $
    run "a := 3; if a = 3 then b := 5 else b := 6" `shouldBe` Just [("a", 3), ("b", 5)]
  describe "while" $
    it "computes the sum of 1 to 10" $
    run "sum := 0; i := 1; while i <= 10 (sum := sum + i; i := i + 1)" `shouldBe` Just [("i", 11), ("sum", 55)]

-- | Run the interpreter
run :: String -> Maybe State
run s = case evalS (parse s) emptyState of
          Nothing -> Nothing
          Just s  -> Just (sort s)
  where
    parse :: String -> Stm
    parse = fromJust . parseWhile
