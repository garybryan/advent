module Advent2021.Day3.Part2Spec (spec) where

import Advent2021.Day3.Part2
import Test.Hspec

spec :: Spec
spec = do
  describe "filterOnBit" $ do
    it "filters numbers to ones where the given bit has the given value" $ do
      filterOnBit 1 1 [0, 2, 3, 5, 10] `shouldBe` [2, 3, 10]

  describe "mostCommonInPos" $ do
    it "Gets the most common bit in a given position in a list of numbers" $ do
      mostCommonInPos 0 [1, 2, 3] `shouldBe` 1
      mostCommonInPos 1 [1, 2, 3] `shouldBe` 1
      mostCommonInPos 2 [1, 2, 3] `shouldBe` 0

    it "Gives 1 if 1 and 0 are equally common" $ do
      mostCommonInPos 1 [1, 2, 3, 4] `shouldBe` 1
