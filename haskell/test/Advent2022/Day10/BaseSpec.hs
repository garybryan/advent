module Advent2022.Day10.BaseSpec (spec) where

import Advent2022.Day10.Base
import Test.Hspec

spec :: Spec
spec = do
  describe "execInstrs" $ do
    it "Executes a noop in one tick, keeping the register value the same" $ do
      execInstrs 5 [noop] `shouldBe` [5, 5]

    it "Executes an addition in two ticks" $ do
      execInstrs 5 [addx 7] `shouldBe` [5, 5, 12]

    it "Executes a series of instructions" $ do
      execInstrs 1 [noop, addx 3, addx (-5)] `shouldBe` [1, 1, 1, 4, 4, -1]

  describe "valuesWithTicks" $ do
    it "Executes instructions and gives the tick number and register value at each tick" $ do
      valuesWithTicks [noop, addx 3, addx (-5)] `shouldBe` [(1, 1), (2, 1), (3, 1), (4, 4), (5, 4), (6, -1)]
