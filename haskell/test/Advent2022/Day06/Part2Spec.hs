module Advent2022.Day06.Part2Spec (spec) where

import Advent2022.Day06.Part2
import Test.Hspec

spec :: Spec
spec = do
  describe "indexAfterMarker" $ do
    it "Finds the end index of prefix of length 14" $ do
      indexAfterMarker "bvwbjplbgvbhsrlpgdmjqwftvncz" `shouldBe` 23
      indexAfterMarker "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" `shouldBe` 29
