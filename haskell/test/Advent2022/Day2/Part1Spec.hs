module Advent2022.Day2.Part1Spec (spec) where

import Advent2022.Day2.Part1
import Test.Hspec

spec :: Spec
spec = do
  describe "scoreFromLinesPart1" $ do
    it "calculates the total score" $ do
      scoreFromLinesPart1 ["A Y", "B X", "C Z"] `shouldBe` 15
