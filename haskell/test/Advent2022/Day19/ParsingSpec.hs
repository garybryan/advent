module Advent2022.Day19.ParsingSpec (spec) where

import qualified Data.Vector as V
import Advent2022.Day19.BaseSpec (v)
import Advent2022.Day19.Parsing
import Test.Hspec

spec :: Spec
spec = do
  describe "parseLine" $ do
    it "gives the blueprint number and costs vector for a line" $ do
      parseLine "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 4 ore. Each obsidian robot costs 4 ore and 8 clay. Each geode robot costs 2 ore and 15 obsidian."
        `shouldBe`
          (1, v [
                v [4, 0, 0, 0],
                v [4, 0, 0, 0],
                v [4, 8, 0, 0],
                v [2, 0, 15, 0]
              ])
