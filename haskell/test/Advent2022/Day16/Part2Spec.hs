module Advent2022.Day16.Part2Spec (spec) where

import Advent2022.Day16.BaseSpec (valveMap)
import Advent2022.Day16.Part2
import qualified Data.IntSet as IntSet
import Test.Hspec

spec :: Spec
spec = do
  describe "partitions" $ do
    it "finds all partitions of length at least 1 of a set" $ do
      partitions (IntSet.fromList [1, 2, 3, 4])
        `shouldBe` [ (IntSet.fromList [1], IntSet.fromList [2, 3, 4]),
                     (IntSet.fromList [2], IntSet.fromList [1, 3, 4]),
                     (IntSet.fromList [1, 2], IntSet.fromList [3, 4]),
                     (IntSet.fromList [3], IntSet.fromList [1, 2, 4]),
                     (IntSet.fromList [1, 3], IntSet.fromList [2, 4]),
                     (IntSet.fromList [2, 3], IntSet.fromList [1, 4]),
                     (IntSet.fromList [1, 2, 3], IntSet.fromList [4])
                   ]

  describe "pairTotal" $ do
    it "finds the total pressure from two runs with different sets of valves" $ do
      pairTotal 26 valveMap 1 (IntSet.fromList [10, 2, 3], IntSet.fromList [4, 8, 5]) `shouldBe` 1707

  describe "pairsMax" $ do
    it "finds the max pressure from any two runs opening different sets of valves" $ do
      pairsMax 26 valveMap 1 `shouldBe` 1707
