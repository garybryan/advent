module Advent2022.Day08.Part2Spec (spec) where

import Advent2022.Day08.BaseSpec hiding (spec)
import Advent2022.Day08.Part2
import Data.Bifunctor (second)
import qualified Data.Matrix as M
import Test.Hspec

spec :: Spec
spec = do
  describe "scenicScoreDir" $ do
    let nextTreeUp = second (subtract 1)

    it "gets the upwards scenic score for an internal tree" $ do
      scenicScoreDir trees (4, 3) 5 nextTreeUp `shouldBe` 2

    it "gets an upwards scenic score of zero for a tree on the top edge" $ do
      scenicScoreDir trees (4, 1) 3 nextTreeUp `shouldBe` 0

  describe "scenicScore" $ do
    it "gets the scenic score for an internal tree" $ do
      scenicScore trees (2, 3) 5 `shouldBe` 4

    it "gets a scenic score of zero for a tree on an edge" $ do
      scenicScore trees (2, 1) 5 `shouldBe` 0

  describe "scenicScores" $ do
    it "gets the scenic score for every tree in a matrix" $ do
      M.toLists (scenicScores trees)
        `shouldBe` [ [0, 0, 0, 0, 0],
                     [0, 1, 4, 1, 0],
                     [0, 6, 1, 2, 0],
                     [0, 1, 8, 3, 0],
                     [0, 0, 0, 0, 0]
                   ]

  describe "maxScenicScore" $ do
    it "gets the highest scenic score for a matrix" $ do
      maxScenicScore trees `shouldBe` 8
