module Advent2021.Day3.Part2Spec (spec) where

import Advent2021.Day3.Part2
import Test.Hspec

spec :: Spec
spec = do
  describe "filterOnBit" $ do
    it "filters numbers to ones where the given bit has the given value" $ do
      filterOnBit 1 1 [0, 2, 3, 5, 10] `shouldBe` [2, 3, 10]

  describe "mostCommonInPos" $ do
    it "gets the most common bit in a given position in a list of numbers" $ do
      mostCommonInPos 0 [1, 2, 3] `shouldBe` 1
      mostCommonInPos 1 [1, 2, 3] `shouldBe` 1
      mostCommonInPos 2 [1, 2, 3, 4] `shouldBe` 0

    it "gives 1 if 1 and 0 are equally common" $ do
      mostCommonInPos 1 [1, 2, 3, 4] `shouldBe` 1

  describe "filterToMostCommonInPos" $ do
    it "filters numbers to the ones with the most common bit in the given position" $ do
      filterToMostCommonInPos 0 [1, 2, 3] `shouldBe` [1, 3]
      filterToMostCommonInPos 1 [1, 2, 3, 4] `shouldBe` [2, 3]
      filterToMostCommonInPos 2 [1, 2, 3, 4] `shouldBe` [1, 2, 3]

  describe "filterToLeastCommonInPos" $ do
    it "filters numbers to the ones with the least common bit in the given position" $ do
      filterToLeastCommonInPos 0 [1, 2, 3] `shouldBe` [2]
      filterToLeastCommonInPos 1 [1, 2, 3, 4] `shouldBe` [1, 4]
      filterToLeastCommonInPos 2 [1, 2, 3, 4] `shouldBe` [4]

  describe "oxygenRating" $ do
    it "Filters by the most common bit value starting at MSB until one number is left" $ do
      oxygenRating 5 [4, 30, 22, 23, 21, 15, 7, 28, 16, 25, 2, 10] `shouldBe` 23

  describe "co2Rating" $ do
    it "filters by the least common bit value starting at MSB until one number is left" $ do
      co2Rating 5 [4, 30, 22, 23, 21, 15, 7, 28, 16, 25, 2, 10] `shouldBe` 10

  describe "lifeSupportRating" $ do
    it "multiplies oxygen and CO2 ratings" $ do
      lifeSupportRating 5 [4, 30, 22, 23, 21, 15, 7, 28, 16, 25, 2, 10] `shouldBe` 230
