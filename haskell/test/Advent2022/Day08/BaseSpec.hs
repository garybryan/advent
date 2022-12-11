module Advent2022.Day08.BaseSpec (trees, spec) where

import Advent2022.Day08.Base
import qualified Data.Matrix as M
import Test.Hspec

trees :: TreeMatrix
trees =
  M.fromLists
    [ [3, 0, 3, 7, 3],
      [2, 5, 5, 1, 2],
      [6, 5, 3, 3, 2],
      [3, 3, 5, 4, 9],
      [3, 5, 3, 9, 0]
    ]

spec :: Spec
spec = do
  describe "parseLines" $ do
    it "parses lines into a tree matrix" $ do
      parseLines
        [ "30373",
          "25512",
          "65332",
          "33549",
          "35390"
        ]
        `shouldBe` trees
