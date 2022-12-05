module Advent2022.Day05.Part2Spec (spec) where

import Advent2022.Day05.BaseSpec (listsToStacks, stacksToLists)
import Advent2022.Day05.Part2
import Test.Hspec

spec :: Spec
spec = do
  describe "moveSeveralAtOnce" $ do
    it "moves several entries at once from one stack to another" $ do
      let stacks = listsToStacks ["NZ", "DCM", "P"]
      let moved = moveSeveralAtOnce (2, 2, 1) stacks
      stacksToLists moved `shouldBe` ["NZCM", "D", "P"]

  describe "applyMovesToLinesSeveralAtOnce" $ do
    it "Applies all moves to the stacks" $ do
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
      stacksToLists (applyMovesToLinesSeveralAtOnce ls) `shouldBe` ["M", "C", "PZND"]
