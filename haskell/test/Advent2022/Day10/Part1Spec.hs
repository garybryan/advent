module Advent2022.Day10.Part1Spec (spec) where

import Advent2022.Day10.Base
import Advent2022.Day10.Part1
import Test.Hspec

spec :: Spec
spec = do
  describe "signalStrengths" $ do
    it "Returns the values of the register multiplied by the cycle number during the given cycle numbers" $ do
      let instrs =
            [ addx 15,
              addx (-11),
              noop,
              addx (-3),
              addx 5,
              noop
            ]
      signalStrengths [2, 6, 10] instrs `shouldBe` [2, 30, 70]

  describe "signalStrengthFromLines" $ do
    it "Executes instructions and returns the total signal strength" $ do
      let ss =
            [ "addx 15",
              "addx -11",
              "addx 6",
              "addx -3",
              "addx 5",
              "addx -1",
              "addx -8",
              "addx 13",
              "addx 4",
              "noop",
              "addx -1",
              "addx 5",
              "addx -1",
              "addx 5",
              "addx -1",
              "addx 5",
              "addx -1",
              "addx 5",
              "addx -1",
              "addx -35",
              "addx 1",
              "addx 24",
              "addx -19",
              "addx 1",
              "addx 16",
              "addx -11",
              "noop",
              "noop",
              "addx 21",
              "addx -15",
              "noop",
              "noop",
              "addx -3",
              "addx 9",
              "addx 1",
              "addx -3",
              "addx 8",
              "addx 1",
              "addx 5",
              "noop",
              "noop",
              "noop",
              "noop",
              "noop",
              "addx -36",
              "noop",
              "addx 1",
              "addx 7",
              "noop",
              "noop",
              "noop",
              "addx 2",
              "addx 6",
              "noop",
              "noop",
              "noop",
              "noop",
              "noop",
              "addx 1",
              "noop",
              "noop",
              "addx 7",
              "addx 1",
              "noop",
              "addx -13",
              "addx 13",
              "addx 7",
              "noop",
              "addx 1",
              "addx -33",
              "noop",
              "noop",
              "noop",
              "addx 2",
              "noop",
              "noop",
              "noop",
              "addx 8",
              "noop",
              "addx -1",
              "addx 2",
              "addx 1",
              "noop",
              "addx 17",
              "addx -9",
              "addx 1",
              "addx 1",
              "addx -3",
              "addx 11",
              "noop",
              "noop",
              "addx 1",
              "noop",
              "addx 1",
              "noop",
              "noop",
              "addx -13",
              "addx -19",
              "addx 1",
              "addx 3",
              "addx 26",
              "addx -30",
              "addx 12",
              "addx -1",
              "addx 3",
              "addx 1",
              "noop",
              "noop",
              "noop",
              "addx -9",
              "addx 18",
              "addx 1",
              "addx 2",
              "noop",
              "noop",
              "addx 9",
              "noop",
              "noop",
              "noop",
              "addx -1",
              "addx 2",
              "addx -37",
              "addx 1",
              "addx 3",
              "noop",
              "addx 15",
              "addx -21",
              "addx 22",
              "addx -6",
              "addx 1",
              "noop",
              "addx 2",
              "addx 1",
              "noop",
              "addx -10",
              "noop",
              "noop",
              "addx 20",
              "addx 1",
              "addx 2",
              "addx 2",
              "addx -6",
              "addx -11",
              "noop",
              "noop",
              "noop"
            ]
      signalStrengthFromLines ss `shouldBe` 13140