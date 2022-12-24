module Advent2022.Day17.BaseSpec (spec) where

import Advent2022.Day17.Base
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import Test.Hspec

spec :: Spec
spec = do
  let settled =
        Map.fromList
          [ (2, IntSet.fromList [3, 4, 5]),
            (3, IntSet.fromList [4])
          ]
  let jetMoves =
        [ JetRight,
          JetRight,
          JetRight,
          JetLeft,
          JetLeft,
          JetRight,
          JetLeft,
          JetRight,
          JetLeft
        ]

  describe "parseLine" $ do
    it "parses a line of jet moves" $ do
      parseLine "<><" `shouldBe` [JetLeft, JetRight, JetLeft]

  describe "jetPush" $ do
    it "pushes a block left" $ do
      jetPush Map.empty [(2, 2), (3, 2)] JetLeft `shouldBe` [(1, 2), (2, 2)]

    it "pushes a block right" $ do
      jetPush Map.empty [(2, 2), (3, 2)] JetRight `shouldBe` [(3, 2), (4, 2)]

    it "doesn't push a block left if it's already against the left wall" $ do
      jetPush Map.empty [(1, 2), (2, 2)] JetLeft `shouldBe` [(1, 2), (2, 2)]

    it "doesn't push a block right if it's already against the right wall" $ do
      jetPush Map.empty [(6, 2), (7, 2)] JetRight `shouldBe` [(6, 2), (7, 2)]

  describe "isOnSettled" $ do
    it "is true if any point in the block would be on a settled point" $ do
      isOnSettled settled [(3, 2), (3, 3)] `shouldBe` True

    it "is false if no point in the block would be on a settled point" $ do
      isOnSettled settled [(6, 3), (6, 4)] `shouldBe` False

  describe "fallBlock" $ do
    it "Falls and pushes a block until settled on the floor, leaving remaining jet moves" $ do
      fallBlock Map.empty (head blocks) jetMoves
        `shouldBe` ( [JetLeft, JetRight, JetLeft, JetRight, JetLeft],
                     Map.fromList
                       [ (1, IntSet.fromList [3, 4, 5, 6])
                       ]
                   )

    it "Falls and pushes a block until settled on top of another, leaving remaining jet moves" $ do
      let settled =
            Map.fromList
              [ (1, IntSet.fromList [3, 4, 5, 6])
              ]
      fallBlock settled (blocks !! 1) (drop 4 jetMoves)
        `shouldBe` ( [JetLeft],
                     Map.fromList
                       [ (1, IntSet.fromList [3, 4, 5, 6]),
                         (2, IntSet.fromList [4]),
                         (3, IntSet.fromList [3, 4, 5]),
                         (4, IntSet.fromList [4])
                       ]
                   )

  describe "fallBlocks" $ do
    it "Falls and pushes several blocks" $ do
      fallBlocks (take 2 blocks) jetMoves
        `shouldBe` Map.fromList
          [ (1, IntSet.fromList [3, 4, 5, 6]),
            (2, IntSet.fromList [4]),
            (3, IntSet.fromList [3, 4, 5]),
            (4, IntSet.fromList [4])
          ]

  describe "heightAfterBlocks" $ do
    -- TOOD fix; this is way off
    it "Runs for 2022 blocks and produces the correct max height" $ do
      let jms = parseLine ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
      heightAfterBlocks 2022 jms `shouldBe` 3068
