module Advent2022.Day16.Part2Spec (spec) where

import Advent2022.Day16.BaseSpec (valves)
import Advent2022.Day16.Part2
import Test.Hspec

spec :: Spec
spec = do
  describe "pairs" $ do
    it "finds all unique pairs from a list of items" $ do
      pairs [1, 2, 3] `shouldBe` ([(1, 2), (1, 3), (2, 3)] :: [(Int, Int)])

  describe "pairsMax" $ do
    it "finds the max pressure from any two runs opening different sets of valves" $ do
      pairsMax 26 valves 0 `shouldBe` 1707
