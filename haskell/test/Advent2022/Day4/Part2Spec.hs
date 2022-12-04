module Advent2022.Day4.Part2Spec (spec) where

import Advent2022.Day4.Part2
import Test.Hspec

spec :: Spec
spec = do
  describe "overlap" $ do
    it "returns true when the second range comes after and overlaps the first" $ do
      overlap (5, 7) (7, 9) `shouldBe` True

    it "returns true when the first range comes after and overlaps the second" $ do
      overlap (5, 7) (3, 6) `shouldBe` True

    it "returns true when the first range contains the second" $ do
      overlap (2, 8) (3, 7) `shouldBe` True

    it "returns true when the second range contains the first" $ do
      overlap (6, 6) (4, 6) `shouldBe` True

    it "returns false when the second range comes after and doesn't overlap the first" $ do
      overlap (2, 6) (7, 8) `shouldBe` False

    it "returns false when the first range comes after and doesn't overlap the second" $ do
      overlap (7, 9) (1, 3) `shouldBe` False

  describe "numOverlap" $ do
    it "returns the number of ranges with overlaps" $ do
      numOverlap [((2, 4), (6, 8)), ((2, 3), (4, 5)), ((5, 7), (7, 9)), ((2, 8), (3, 7)), ((6, 6), (4, 6)), ((2, 6), (4, 8))] `shouldBe` 4

