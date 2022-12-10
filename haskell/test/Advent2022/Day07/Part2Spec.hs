module Advent2022.Day07.Part2Spec (spec) where

import Advent2022.Day07.Base
import Advent2022.Day07.Part2
import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.Hspec

spec :: Spec
spec = do
  let oneLevelFs =
        FS
          { fsMap =
              Map.fromList
                [ (0, DirNode "/" $ Set.fromList [1, 2]),
                  (1, FileNode "a.txt" 25),
                  (2, FileNode "b.txt" 15)
                ],
            nextKeys = [3 .. 50]
          }

  let twoLevelFs =
        FS
          { fsMap =
              Map.fromList
                [ (0, DirNode "/" $ Set.fromList [1, 3, 5]),
                  (1, DirNode "biggerstuff" $ Set.fromList [2]),
                  (2, FileNode "a.txt" 28000000),
                  (3, DirNode "smallerstuff" $ Set.fromList [4]),
                  (4, FileNode "b.txt" 22000000),
                  (5, FileNode "toosmall" 12000000),
                  (6, FileNode "c.txt" 12000000)
                ],
            nextKeys = [5 .. 50]
          }

  describe "unusedSpace" $ do
    it "Calculates the unused space for a filesystem" $ do
      unusedSpace twoLevelFs `shouldBe` 8000000

  describe "spaceToFree" $ do
    it "Calculates how much space must be freed to run the update" $ do
     spaceToFree twoLevelFs `shouldBe` 22000000

    it "Gives zero if there is already enough space to run the update" $ do
     spaceToFree oneLevelFs `shouldBe` 0

  describe "dirsToMakeSpace" $ do
    it "Gives the directories and their sizes that could be deleted to make space for the update" $ do
      Set.fromList (dirsToMakeSpace twoLevelFs) `shouldBe` Set.fromList [
          ("/", 62000000),
          ("biggerstuff", 28000000),
          ("smallerstuff", 22000000)
        ]

  describe "smallestDirToMakeSpace" $ do
    it "Gives the size of the smallest directory that could be deleted to make space for the update" $ do
      smallestDirToMakeSpace twoLevelFs `shouldBe` 22000000
