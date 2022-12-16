module Advent2022.Day15.Part2Spec (spec) where

import Advent2022.Day15.BaseSpec (ssAndBs)
import Advent2022.Day15.Part2
import Test.Hspec

spec :: Spec
spec = do
  describe "distressBeaconPos" $ do
    it "finds the first position that cannot contain a beacon" $ do
      distressBeaconPos (0, 20) ssAndBs `shouldBe` (14, 11)

  describe "tuningFreq" $ do
    it "is the point's x coordinate multiplied by the multiplier plus its y coordinate" $ do
      tuningFreq 4000000 (14, 11) `shouldBe` 56000011
