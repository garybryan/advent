module Advent2022.Day06.BaseSpec (spec) where

import Advent2022.Day06.Base
import Test.Hspec

spec :: Spec
spec = do
  describe "indexAfterUniquePrefix" $ do
    it "Finds the end index of the first unique prefix of given length" $ do
      indexAfterUniquePrefix 4 "bvwbjplbgvbhsrlpgdmjqwftvncz" `shouldBe` 5
      indexAfterUniquePrefix 4 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" `shouldBe` 10
      indexAfterUniquePrefix 14 "bvwbjplbgvbhsrlpgdmjqwftvncz" `shouldBe` 23
      indexAfterUniquePrefix 14 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" `shouldBe` 29

  describe "parseLines" $ do
    it "Gives the first line of the input" $ do
      parseLines ["abc", "", "xyz"] `shouldBe` "abc"
