module Advent2021.Day3.Part2Spec (spec) where

import Advent2021.Day3.Part2
import Test.Hspec

spec :: Spec
spec = do
  describe "filterOnBit" $ do
    it "filters numbers to ones where the given bit has the given value" $ do
      filterOnBit 1 1 [0, 2, 3, 5, 10] `shouldBe` [2, 3, 10]
