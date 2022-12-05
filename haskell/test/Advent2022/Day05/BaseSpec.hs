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
  describe "parseStack" $ do
    it "parses a stack line into stacks" $ do
      let stackLine = "[V]     [B]                     [C]"
      stacksToLists (parseStack stackLine) `shouldBe` ["V", "", "B", "", "", "", "", "", "C"]

  describe "parseStacks" $ do
    it "parses stack lines into stacks" $ do
      let stackLines =
            [ "    [D]    ",
              "[N] [C]    ",
              "[Z] [M] [P]"
            ]
      stacksToLists (parseStacks stackLines) `shouldBe` ["ZN", "MCD", "P"]

  describe "parseMove" $ do
    it "parses a line into a move tuple (num, from, to)" $ do
      parseMove "move 1 from 2 to 3" `shouldBe` (1, 2, 3)

  describe "topCrates" $ do
    it "Lists the top crates on the stacks" $ do
      topCrates (listsToStacks ["ZN", "MCD", "P"]) `shouldBe` "NDP"

    it "Lists the top crates on the stacks when a stack is empty" $ do
      topCrates (listsToStacks ["ZN", "", "P"]) `shouldBe` "N P"

  describe "applyMovesToLines" $ do
    it "applies all moves to the stacks using the given function" $ do
      let ls =
            [ "    [D]    ",
              "[N] [C]    ",
              "[Z] [M] [P]",
              " 1   2   3 ",
              "",
              "move 1 from 2 to 1"
            ]
      let f (_, _, _) = V.reverse
      stacksToLists (applyMovesToLines f ls) `shouldBe` ["P", "MCD", "ZN"]
