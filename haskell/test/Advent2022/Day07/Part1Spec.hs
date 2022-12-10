module Advent2022.Day07.Part1Spec (spec) where

import Advent2022.Day07.Part1
import Test.Hspec

spec :: Spec
spec = do
  describe "totalUnder100k" $ do
    it "Finds the total of directories with a total size of up to 100,000" $ do
      let input =
            [ "$ cd /",
              "$ ls",
              "dir a",
              "14848514 b.txt",
              "8504156 c.dat",
              "dir d",
              "$ cd a",
              "$ ls",
              "dir e",
              "29116 f",
              "2557 g",
              "62596 h.lst",
              "$ cd e",
              "$ ls",
              "584 i",
              "$ cd ..",
              "$ cd ..",
              "$ cd d",
              "$ ls",
              "4060174 j",
              "8033020 d.log",
              "5626152 d.ext",
              "7214296 k"
            ]
      totalUnder100k [1 .. 50] input `shouldBe` 95437
