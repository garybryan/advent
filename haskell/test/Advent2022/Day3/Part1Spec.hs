module Advent2022.Day3.Part1Spec (spec) where

import Advent2022.Day3.Part1
import Control.Exception (evaluate)
import Test.Hspec

spec :: Spec
spec = do
  describe "commonCompartmentItemPriority" $ do
    it "Finds the priority of the common item" $ do
      commonCompartmentItemPriority "vJrwpWtwJgWrhcsFMMfFFhFpb" `shouldBe` 16
      commonCompartmentItemPriority "PmmdzqPrVvPwwTWBwg" `shouldBe` 42

  describe "commonCompartmentItemsPriority" $ do
    it "Finds the sum of priorities of the common item in each rucksack" $ do
      -- ghc gives a syntax error when I try to spread this over multiple lines...
      let rucksacks = ["vJrwpWtwJgWrhcsFMMfFFhFp", "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL", "PmmdzqPrVvPwwTWBwg", "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn", "ttgJtRGJQctTZtZT", "CrZsJsPPZsGzwwsLwLmpwMDw"]
      commonCompartmentItemsPriority rucksacks `shouldBe` 157
