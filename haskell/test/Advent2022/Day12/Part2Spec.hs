module Advent2022.Day12.Part2Spec (spec) where

import Advent2022.Day12.BaseSpec (hills, ls)
import Advent2022.Day12.Part2
import qualified Data.Set as Set
import Test.Hspec

spec :: Spec
spec = do
  describe "startPoints" $ do
    it "finds all possible starting points" $ do
      Set.fromList (startPoints hills) `shouldBe` Set.fromList [(1, 1), (1, 2), (3, 1), (4, 1), (5, 1), (2, 1)]

  describe "bfsAllLines" $ do
    it "finds the shortest path from all possible start points to the end" $ do
      bfsAllLines ls `shouldBe` 29
