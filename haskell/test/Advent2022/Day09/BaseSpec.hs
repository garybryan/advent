module Advent2022.Day09.BaseSpec (spec) where

import Advent2022.Day09.Base
import Data.Bifunctor (first)
import qualified Data.Set as Set
import Test.Hspec

spec :: Spec
spec = do
  let ss =
        [ "R 4",
          "U 4",
          "L 3",
          "D 1",
          "R 4",
          "D 1",
          "L 5",
          "R 2"
        ]

  describe "moveTail" $ do
    describe "tail touching head" $ do
      it "doesn't move the tail when it is overlapping the head" $ do
        moveTail (1, 1) (1, 1) `shouldBe` (1, 1)

      it "doesn't move the tail when it is horizontally adjacent to the head" $ do
        moveTail (1, 1) (0, 1) `shouldBe` (0, 1)
        moveTail (1, 1) (2, 1) `shouldBe` (2, 1)

      it "doesn't move the tail when it is vertically adjacent to the head" $ do
        moveTail (1, 1) (1, 2) `shouldBe` (1, 2)
        moveTail (1, 1) (1, 0) `shouldBe` (1, 0)

      it "doesn't move the tail when it is diagonally adjacent to the head" $ do
        moveTail (1, 1) (0, 0) `shouldBe` (0, 0)
        moveTail (1, 1) (2, 2) `shouldBe` (2, 2)
        moveTail (1, 1) (2, 0) `shouldBe` (2, 0)
        moveTail (1, 1) (0, 2) `shouldBe` (0, 2)

    describe "tail not touching head" $ do
      it "moves the tail when it is left of the head" $ do
        moveTail (3, 1) (1, 1) `shouldBe` (2, 1)

      it "moves the tail when it is below the head" $ do
        moveTail (1, 3) (1, 1) `shouldBe` (1, 2)

      it "moves the tail when it is right of the head" $ do
        moveTail (1, 1) (3, 1) `shouldBe` (2, 1)

      it "moves the tail when it is above the head" $ do
        moveTail (1, 1) (1, 3) `shouldBe` (1, 2)

      it "moves the tail when it is top right of the head" $ do
        moveTail (1, 1) (2, 3) `shouldBe` (1, 2)
        moveTail (1, 1) (3, 2) `shouldBe` (2, 1)

      it "moves the tail when it is top left of the head" $ do
        moveTail (1, 1) (0, 3) `shouldBe` (1, 2)
        moveTail (1, 1) (-1, 2) `shouldBe` (0, 1)

      it "moves the tail when it is bottom right of the head" $ do
        moveTail (2, 2) (3, 0) `shouldBe` (2, 1)
        moveTail (2, 2) (4, 1) `shouldBe` (3, 2)

      it "moves the tail when it is bottom left of the head" $ do
        moveTail (2, 2) (1, 0) `shouldBe` (2, 1)
        moveTail (2, 2) (0, 1) `shouldBe` (1, 2)

  describe "applyMoveAndUpdate" $ do
    it "Applies a move and updates the tail position set" $ do
      applyMoveAndUpdate
        ( first (+ 1)
        )
        ( [ (1, 1),
            (0, 1)
          ],
          Set.fromList [(0, 1)]
        )
        `shouldBe` ([(2, 1), (1, 1)], Set.fromList [(0, 1), (1, 1)])

  describe "applyLines" $ do
    it "Applies a list of move lines with a 2-knot rope and gives the final points and set of tail points" $ do
      applyLines 2 ss
        `shouldBe` ( [(2, 2), (1, 2)],
                     Set.fromList
                       [ (0, 0),
                         (1, 0),
                         (2, 0),
                         (3, 0),
                         (4, 1),
                         (1, 2),
                         (2, 2),
                         (3, 2),
                         (4, 2),
                         (3, 3),
                         (4, 3),
                         (2, 4),
                         (3, 4)
                       ]
                   )

    it "Applies a list of move lines with a 5-knot rope and gives the final points and set of tail points" $ do
      applyLines 5 ss
        `shouldBe` ( [ (2, 2),
                       (1, 2),
                       (2, 2),
                       (3, 2),
                       (2, 2)
                     ],
                     Set.fromList
                       [ (0, 0),
                         (1, 1),
                         (2, 2)
                       ]
                   )

  describe "numTailVisited" $ do
    it "Applies a list of move lines with a 2-knot rope and gives the number of points the tail visited" $ do
      numTailVisited 2 ss `shouldBe` 13

    it "Applies a list of move lines with a 5-knot rope and gives the number of points the tail visited" $ do
      numTailVisited 5 ss `shouldBe` 3
