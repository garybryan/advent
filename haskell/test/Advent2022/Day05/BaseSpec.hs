module Advent2022.Day05.BaseSpec (stacksToLists, listsToStacks, spec) where

import Advent2022.Day05.Base
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
  describe "parseStackLine" $ do
    it "parses a stack line into stacks" $ do
      let stackLine = "[V]     [B]                     [C]"
      stacksToLists (parseStackLine stackLine) `shouldBe` ["V", "", "B", "", "", "", "", "", "C"]

  describe "parseStackLines" $ do
    it "parses stack lines into stacks" $ do
      let stackLines = ["[V]     [B]                     [C]", "[C]     [N] [G]         [W]     [P]"]
      stacksToLists (parseStackLines stackLines) `shouldBe` ["CV", "", "NB", "G", "", "", "W", "", "PC"]

  -- TODO
  -- describe "parseStackLines" $ do
  --   it "parses stack lines into stacks when uneven" $ do

  describe "parseMove" $ do
    it "parses a line into a move tuple (num, from, to)" $ do
      parseMove "move 1 from 2 to 3" `shouldBe` (1, 2, 3)

  describe "topCrates" $ do
    it "Lists the top crates on the stacks" $ do
      topCrates (listsToStacks ["ZN", "MCD", "P"]) `shouldBe` "NDP"

    it "Lists the top crates on the stacks when a stack is empty" $ do
      topCrates (listsToStacks ["ZN", "", "P"]) `shouldBe` "N P"
