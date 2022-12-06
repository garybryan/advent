module Advent2022.Day06.Part1Spec (spec) where

import Advent2022.Day06.Part1
import Test.Hspec

spec :: Spec
spec = do
  describe "indexAfterMarker" $ do
    it "Finds the end index of prefix of length 4" $ do
      indexAfterMarker "bvwbjplbgvbhsrlpgdmjqwftvncz" `shouldBe` 5
      indexAfterMarker "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" `shouldBe` 10
