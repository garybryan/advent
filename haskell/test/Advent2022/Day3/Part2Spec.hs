module Advent2022.Day3.Part2Spec (spec) where

import Advent2022.Day3.Part2
import Test.Hspec

spec :: Spec
spec = do
  describe "priorityGroupsOf3" $ do
    it "Finds the first common items in multiple lists" $ do
      let rucksacks = ["vJrwpWtwJgWrhcsFMMfFFhFp", "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL", "PmmdzqPrVvPwwTWBwg", "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn", "ttgJtRGJQctTZtZT", "CrZsJsPPZsGzwwsLwLmpwMDw"]
      priorityGroupsOf3 rucksacks `shouldBe` 70
