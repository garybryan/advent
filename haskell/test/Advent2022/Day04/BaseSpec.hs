module Advent2022.Day04.BaseSpec (spec) where

import Advent2022.Day04.Base
import Test.Hspec

spec :: Spec
spec = do
  describe "parseLine" $ do
    it "parses a pair of ranges from a line" $ do
      parseLine "2-5,8-13" `shouldBe` ((2, 5), (8, 13))
