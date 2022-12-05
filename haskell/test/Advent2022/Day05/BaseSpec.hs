module Advent2022.Day05.BaseSpec (spec) where

import Advent2022.Day05.Base
import Test.Hspec

spec :: Spec
spec = do
  describe "square" $ do
    it "squares a number" $ do
      square 4 `shouldBe` 16
