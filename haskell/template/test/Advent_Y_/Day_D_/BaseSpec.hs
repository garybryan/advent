module Advent_Y_.Day_D_.BaseSpec (spec) where

import Advent_Y_.Day_D_.Base
import Test.Hspec

spec :: Spec
spec = do
  describe "square" $ do
    it "squares a number" $ do
      square 4 `shouldBe` 16
