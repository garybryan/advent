module Advent2022.Day05.Part1Spec (spec) where

import Advent2022.Day05.BaseSpec (listsToStacks, stacksToLists)
import Advent2022.Day05.Part1
import Test.Hspec

spec :: Spec
spec = do
  describe "move" $ do
    it "moves an entry from one stack to another" $ do
      let stacks = listsToStacks ["NZ", "DCM", "P"]
      let moved = move 1 3 stacks
      stacksToLists moved `shouldBe` ["N", "DCM", "PZ"]

  describe "moveSeveral" $ do
    it "moves several entries from one stack to another" $ do
      let stacks = listsToStacks ["NZ", "DCM", "P"]
      let moved = moveSeveral 2 2 1 stacks
      stacksToLists moved `shouldBe` ["NZMC", "D", "P"]

  describe "applyMoves" $ do
    it "Applies a list of moves to a stack" $ do
      let stacks = listsToStacks ["ZN", "MCD", "P"]
      let moves = [(1, 2, 1), (3, 1, 3), (2, 2, 1), (1, 1, 2)]
      stacksToLists (stacks `applyMoves` moves) `shouldBe` ["C", "M", "PDNZ"]

  describe "applyMovesToLines" $ do
    it "Applies all moves to the stacks, for the moves and stacks defined in the lines" $ do
      let ls =
            [ "    [D]    ",
              "[N] [C]    ",
              "[Z] [M] [P]",
              " 1   2   3 ",
              "",
              "move 1 from 2 to 1",
              "move 3 from 1 to 3",
              "move 2 from 2 to 1",
              "move 1 from 1 to 2"
            ]
      stacksToLists (applyMovesToLines ls) `shouldBe` ["C", "M", "PDNZ"]
