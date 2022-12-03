module Advent2022.Day3Spec (spec) where

import Advent2022.Day3
import Control.Exception (evaluate)
import Test.Hspec

spec :: Spec
spec = do
  describe "priority" $ do
    it "calculates the priority of lower-case letters" $ do
      priority 'c' `shouldBe` 3
      priority 'x' `shouldBe` 24

    it "calculates the priority of upper-case letters" $ do
      priority 'L' `shouldBe` 38
      priority 'P' `shouldBe` 42

    it "throws an error on an invalid character" $ do
      priority 'L' `shouldBe` 38
      evaluate (priority '.') `shouldThrow` errorCall "Invalid character: '.'"

  describe "commonItemPriority" $ do
    it "Finds the priority of the common item" $ do
      commonItemPriority "vJrwpWtwJgWrhcsFMMfFFhFpb" `shouldBe` 16
      commonItemPriority "PmmdzqPrVvPwwTWBwg" `shouldBe` 42
