module Advent2022.Day4.BaseSpec (spec) where

import Advent2022.Day4.Base
import Test.Hspec

spec :: Spec
spec = do
  describe "lineRanges" $ do
    it "parses ranges from a line" $ do
      lineRanges "2-5,8-13" `shouldBe` ((2, 5), (8, 13))
