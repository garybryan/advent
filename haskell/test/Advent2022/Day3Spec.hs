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

  describe "commonCompartmentItemPriority" $ do
    it "Finds the priority of the common item" $ do
      commonCompartmentItemPriority "vJrwpWtwJgWrhcsFMMfFFhFpb" `shouldBe` 16
      commonCompartmentItemPriority "PmmdzqPrVvPwwTWBwg" `shouldBe` 42

  describe "commonCompartmentItemsPriority" $ do
    it "Finds the sum of priorities of the common item in each rucksack" $ do
      -- ghc gives a syntax error when I try to spread this over multiple lines...
      let rucksacks = ["vJrwpWtwJgWrhcsFMMfFFhFp", "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL", "PmmdzqPrVvPwwTWBwg", "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn", "ttgJtRGJQctTZtZT", "CrZsJsPPZsGzwwsLwLmpwMDw"]
      commonCompartmentItemsPriority rucksacks `shouldBe` 157

  describe "priorityGroupsOf3" $ do
    it "Finds the first common items in multiple lists" $ do
      let rucksacks = ["vJrwpWtwJgWrhcsFMMfFFhFp", "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL", "PmmdzqPrVvPwwTWBwg", "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn", "ttgJtRGJQctTZtZT", "CrZsJsPPZsGzwwsLwLmpwMDw"]
      priorityGroupsOf3 rucksacks `shouldBe` 70
