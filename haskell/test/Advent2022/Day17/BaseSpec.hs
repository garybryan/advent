module Advent2022.Day17.BaseSpec (spec) where

import Advent2022.Day17.Base
import Data.Bits (bit)
import qualified Data.Map as Map
import Data.Word (Word8)
import Test.Hspec

-- Utility to avoid having to write bit vectors as numbers.
bitsFor :: [Int] -> Word8
bitsFor = sum . map ((bit :: Int -> Word8) . subtract 1)

spec :: Spec
spec = do
  let settled =
        Map.fromList
          [ (2, bitsFor [3, 4, 5]),
            (3, bitsFor [4])
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
    it "pushes a rock left" $ do
      jetPush Map.empty [(2, 2), (3, 2)] JetLeft `shouldBe` [(1, 2), (2, 2)]

    it "pushes a rock right" $ do
      jetPush Map.empty [(2, 2), (3, 2)] JetRight `shouldBe` [(3, 2), (4, 2)]

    it "doesn't push a rock left if it's already against the left wall" $ do
      jetPush Map.empty [(1, 2), (2, 2)] JetLeft `shouldBe` [(1, 2), (2, 2)]

    it "doesn't push a rock right if it's already against the right wall" $ do
      jetPush Map.empty [(6, 2), (7, 2)] JetRight `shouldBe` [(6, 2), (7, 2)]

  describe "isOnSettled" $ do
    it "is true if any point in the rock would be on a settled point" $ do
      isOnSettled settled [(3, 2), (3, 3)] `shouldBe` True

    it "is false if no point in the rock would be on a settled point" $ do
      isOnSettled settled [(6, 3), (6, 4)] `shouldBe` False

  describe "fallRock" $ do
    it "Falls and pushes a rock until settled on the floor, leaving remaining jet moves" $ do
      fallRock Map.empty (head rocks) jetMoves
        `shouldBe` ( [JetLeft, JetRight, JetLeft, JetRight, JetLeft],
                     Map.fromList
                       [ (1, bitsFor [3, 4, 5, 6])
                       ]
                   )

    it "Falls and pushes a rock until settled on top of another, leaving remaining jet moves" $ do
      let settled =
            Map.fromList
              [ (1, bitsFor [3, 4, 5, 6])
              ]
      fallRock settled (rocks !! 1) (drop 4 jetMoves)
        `shouldBe` ( [JetLeft],
                     Map.fromList
                       [ (1, bitsFor [3, 4, 5, 6]),
                         (2, bitsFor [4]),
                         (3, bitsFor [3, 4, 5]),
                         (4, bitsFor [4])
                       ]
                   )

  describe "fallRocks" $ do
    it "Falls and pushes several rocks" $ do
      fallRocks (take 2 rocks) jetMoves
        `shouldBe` Map.fromList
          [ (1, bitsFor [3, 4, 5, 6]),
            (2, bitsFor [4]),
            (3, bitsFor [3, 4, 5]),
            (4, bitsFor [4])
          ]

  describe "heightAfterRocks" $ do
    it "Runs for 2022 rocks and produces the correct max height" $ do
      let jms = parseLine ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
      heightAfterRocks 2022 jms `shouldBe` 3068
