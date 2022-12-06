module Advent2022.Day06.BaseSpec (spec) where

import Advent2022.Day06.Base
import Test.Hspec

spec :: Spec
spec = do
  describe "indexAfterUniquePrefix" $ do
    it "Finds the end index of the first unique prefix of given length" $ do
      indexAfterUniquePrefix 4 "bvwbjplbgvbhsrlpgdmjqwftvncz" `shouldBe` 5
      indexAfterUniquePrefix 4 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" `shouldBe` 10
      indexAfterUniquePrefix 4 "aabcd" `shouldBe` 5
      indexAfterUniquePrefix 14 "bvwbjplbgvbhsrlpgdmjqwftvncz" `shouldBe` 23
      indexAfterUniquePrefix 14 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" `shouldBe` 29

    it "Returns -1 if there is no unique prefix" $ do
      indexAfterUniquePrefix 4 "abcabca" `shouldBe` -1
      indexAfterUniquePrefix 4 "abdd" `shouldBe` -1

    it "Returns -1 if the string is shorter than the prefix length" $ do
      indexAfterUniquePrefix 4 "a" `shouldBe` -1

  describe "indexAfterUniquePrefixSliding" $ do
    it "Finds the end index of the first unique prefix of given length" $ do
      indexAfterUniquePrefixSliding 4 "bvwbjplbgvbhsrlpgdmjqwftvncz" `shouldBe` 5
      indexAfterUniquePrefixSliding 4 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" `shouldBe` 10
      indexAfterUniquePrefixSliding 4 "aabcd" `shouldBe` 5
      indexAfterUniquePrefixSliding 14 "bvwbjplbgvbhsrlpgdmjqwftvncz" `shouldBe` 23
      indexAfterUniquePrefixSliding 14 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" `shouldBe` 29

    it "Returns -1 if there is no unique prefix" $ do
      indexAfterUniquePrefixSliding 4 "abcabca" `shouldBe` -1
      indexAfterUniquePrefixSliding 4 "abdd" `shouldBe` -1

    it "Returns -1 if the string is shorter than the prefix length" $ do
      indexAfterUniquePrefixSliding 4 "a" `shouldBe` -1

  describe "parseLines" $ do
    it "Gives the first line of the input" $ do
      parseLines ["abc", "", "xyz"] `shouldBe` "abc"
