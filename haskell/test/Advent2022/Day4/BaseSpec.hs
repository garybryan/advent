module Advent2022.Day4.BaseSpec (spec) where

import Advent2022.Day4.Base
import Test.Hspec

spec :: Spec
spec = do
  describe "contains" $ do
    it "returns true when the first range contains the second" $ do
      contains (2, 8) (3, 7) `shouldBe` True

    it "returns true when the second range contains the first" $ do
      contains (6, 6) (4, 6) `shouldBe` True

    it "returns false when the ranges are separate" $ do
      contains (2, 4) (6, 8) `shouldBe` False

    it "returns false when the ranges overlap but none contains the other" $ do
      contains (2, 6) (4, 8) `shouldBe` False

  describe "numContains" $ do
    it "returns the number of pairs where one range contains the other" $ do
      numContains [((2, 4), (6, 8)), ((2, 3), (4, 5)), ((5, 7), (7, 9)), ((2, 8), (3, 7)), ((6, 6), (4, 6)), ((2, 6), (4, 8))] `shouldBe` 2

  describe "lineRanges" $ do
    it "parses ranges from a line" $ do
      lineRanges "2-5,8-13" `shouldBe` ((2, 5), (8, 13))
