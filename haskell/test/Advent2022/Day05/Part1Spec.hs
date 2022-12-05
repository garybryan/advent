module Advent2022.Day05.Part1Spec (spec) where

import Advent2022.Day05.Part1
import qualified Data.Dequeue as DQ
import Data.Foldable (toList)
import qualified Data.Vector as V
import Test.Hspec

stacksToLists :: Stacks -> [String]
stacksToLists = map toList . toList

listsToStacks :: [String] -> Stacks
listsToStacks = V.fromList . map DQ.fromList

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
      let stackLine = "[V]     [B]                     [C]"
      (stacksToLists $ parseStackLine stackLine) `shouldBe` ["V", "", "B", "", "", "", "", "", "C"]

  describe "parseStackLines" $ do
    it "parses stack lines into stacks" $ do
      -- it "parses stack lines into stacks" $ do
      let stackLines = ["[V]     [B]                     [C]", "[C]     [N] [G]         [W]     [P]"]
      (stacksToLists $ parseStackLines stackLines) `shouldBe` ["VC", "", "BN", "G", "", "", "W", "", "CP"]

  -- TODO
  -- describe "parseStackLines" $ do
  --   it "parses stack lines into stacks when uneven" $ do

  describe "parseMove" $ do
    it "parses a line into a move tuple (num, from, to)" $ do
      parseMove "move 1 from 2 to 3" `shouldBe` (1, 2, 3)

  describe "applyMoves" $ do
    it "Applies a list of moves to a stack" $ do
      let stacks = listsToStacks ["ZN", "MCD", "P"]
      let moves = [(1, 2, 1), (3, 1, 3), (2, 2, 1), (1, 1, 2)]
      stacksToLists (stacks `applyMoves` moves) `shouldBe` ["C", "M", "PDNZ"]
