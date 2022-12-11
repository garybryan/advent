module Advent2022.Day08.Part1Spec (spec) where

import Advent2022.Day08.BaseSpec hiding (spec)
import Advent2022.Day08.Part1
import qualified Data.Matrix as M
import Test.Hspec

spec :: Spec
spec = do
  describe "visVectorList" $ do
    it "Makes a visibility list where only the first tree is visible" $ do
      visVectorList (M.getRow 3 trees) `shouldBe` [True, False, False, False, False]

    it "Makes a visibility list where a subsequent tree is visible" $ do
      visVectorList (M.getCol 3 trees) `shouldBe` [True, True, False, False, False]

    it "Makes a visibility list where a tree is visible after one that isn't" $ do
      visVectorList (M.getRow 1 trees) `shouldBe` [True, False, False, True, False]

  describe "leftVisMatrix" $ do
    it "makes a visibility matrix for visibility from the left" $ do
      leftVisMatrix trees
        `shouldBe` M.fromLists
          [ [True, False, False, True, False],
            [True, True, False, False, False],
            [True, False, False, False, False],
            [True, False, True, False, True],
            [True, True, False, True, False]
          ]

  describe "topVisMatrix" $ do
    it "makes a visibility matrix for visibility from the top" $ do
      topVisMatrix trees
        `shouldBe` M.fromLists
          [ [True, True, True, True, True],
            [False, True, True, False, False],
            [True, False, False, False, False],
            [False, False, False, False, True],
            [False, False, False, True, False]
          ]

  describe "rightVisMatrix" $ do
    it "makes a visibility matrix for visibility from the right" $ do
      rightVisMatrix trees
        `shouldBe` M.fromLists
          [ [False, False, False, True, True],
            [False, False, True, False, True],
            [True, True, False, True, True],
            [False, False, False, False, True],
            [False, False, False, True, True]
          ]

  describe "bottomVisMatrix" $ do
    it "makes a visibility matrix for visibility from the bottom" $ do
      bottomVisMatrix trees
        `shouldBe` M.fromLists
          [ [False, False, False, False, False],
            [False, False, False, False, False],
            [True, False, False, False, False],
            [False, False, True, False, True],
            [True, True, True, True, True]
          ]

  describe "visMatrix" $ do
    it "makes a visibility matrix for visibility from all sides" $ do
      visMatrix trees
        `shouldBe` M.fromLists
          [ [True, True, True, True, True],
            [True, True, True, False, True],
            [True, True, False, True, True],
            [True, False, True, False, True],
            [True, True, True, True, True]
          ]

  describe "numVisible" $ do
    it "finds the number of visible trees in a matrix" $ do
      numVisible (visMatrix trees) `shouldBe` 21
