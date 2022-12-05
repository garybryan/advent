module Advent2022.Day05.Part1Spec (spec) where

import Advent2022.Day05.Part1
import qualified Data.Dequeue as DQ
import Data.Foldable (toList)
import qualified Data.Vector as V
import Test.Hspec

stacksToLists :: Stacks -> [[Char]]
stacksToLists = map toList . toList

spec :: Spec
spec = do
  describe "move" $ do
    it "moves an entry from one stack to another" $ do
      let stacks = V.fromList [DQ.fromList ['N', 'Z'], DQ.fromList ['D', 'C', 'M'], DQ.fromList ['P']]
      let moved = move 1 3 stacks
      stacksToLists moved `shouldBe` [['N'], ['D', 'C', 'M'], ['P', 'Z']]

  describe "moveSeveral" $ do
    it "moves several entries from one stack to another" $ do
      let stacks = V.fromList [DQ.fromList ['N', 'Z'], DQ.fromList ['D', 'C', 'M'], DQ.fromList ['P']]
      let moved = moveSeveral 2 2 1 stacks
      stacksToLists moved `shouldBe` [['N', 'Z', 'M', 'C'], ['D'], ['P']]

  describe "parseStackLine" $ do
    it "parses a stack line into stacks" $ do
      (stacksToLists $ parseStackLine "[V]     [B]                     [C]") `shouldBe` ["V", "", "B", "", "", "", "", "", "C"]
