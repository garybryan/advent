module Advent2022.Day12.Part1Spec (spec) where

import Advent2022.Day12.BaseSpec (end, hills, ls)
import Advent2022.Day12.Part1
import Test.Hspec

spec :: Spec
spec = do
  describe "bfsLines" $ do
    it "finds the shortest path from the start to the end" $ do
      bfsLines ls `shouldBe` 31
