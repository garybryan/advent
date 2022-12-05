module Advent2022.Day04.Part1Spec (spec) where

import Advent2022.Day04.Part1
import Test.Hspec

spec :: Spec
spec = do
  describe "hasContainment" $ do
    it "returns true when the first range contain the second" $ do
      hasContainment (2, 8) (3, 7) `shouldBe` True

    it "returns true when the second range contain the first" $ do
      hasContainment (6, 6) (4, 6) `shouldBe` True

    it "returns false when the ranges are separate" $ do
      hasContainment (2, 4) (6, 8) `shouldBe` False

    it "returns false when the ranges overlap but none contain the other" $ do
      hasContainment (2, 6) (4, 8) `shouldBe` False

  describe "numContain" $ do
    it "returns the number of ranges with containment" $ do
      numContain [((2, 4), (6, 8)), ((2, 3), (4, 5)), ((5, 7), (7, 9)), ((2, 8), (3, 7)), ((6, 6), (4, 6)), ((2, 6), (4, 8))] `shouldBe` 2

