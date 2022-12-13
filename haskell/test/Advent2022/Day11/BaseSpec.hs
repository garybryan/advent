module Advent2022.Day11.BaseSpec (spec) where

import Advent2022.Day11.Base
import qualified Data.Dequeue as DQ
import qualified Data.Vector as V
import Lib.Read
import Test.Hspec

-- I've implemented parsing rather than manually entering data, so I'm gonna use it!
withMonkeys :: IO Monkeys
withMonkeys = do
  ss <- readLines "data/2022/11-test.txt"
  return $ linesToMonkeys ss

spec :: Spec
spec = do
  describe "parseItemsLine" $ do
    it "parses to a list of items" $ do
      parseItemsLine "  Starting items: 74, 64, 74, 63, 53" `shouldBe` DQ.fromList [74, 64, 74, 63, 53]

  describe "parseOperationLine" $ do
    it "parses an addition operation" $ do
      let f = parseOperationLine "  Operation: new = old + 6"
      f 0 `shouldBe` 6

    it "parses a multiplication operation" $ do
      let f = parseOperationLine "  Operation: new = old * 5"
      f 6 `shouldBe` 30

    it "parses a squaring operation" $ do
      let f = parseOperationLine "  Operation: new = old * old"
      f 6 `shouldBe` 36

  describe "parseModulusLine" $ do
    it "parses a modulus" $ do
      parseModulusLine "  Test: divisible by 5" `shouldBe` 5

  describe "parseThrowLine" $ do
    it "parses a throw target" $ do
      parseThrowLine "    If true: throw to monkey 1" `shouldBe` 1

  describe "inspectItem" $ do
    before withMonkeys $ do
      it "applies the monkey's operation to the item then floor divides by the divisor" $ \ms -> do
        let m = ms V.! 0
        inspectItem 79 m 3 5000 `shouldBe` 500

      it "applies the monkey's operation to the item with a lower inspection modulus and divisor 1" $ \ms -> do
        let m = ms V.! 0
        inspectItem 79 m 1 100 `shouldBe` 1

  describe "throwTarget" $ do
    before withMonkeys $ do
      it "gives the target to throw to when the item is divisible by the modulus" $ \ms -> do
        let m = ms V.! 2
        throwTarget 2080 m `shouldBe` 1

      it "gives the target to throw to when the item is not divisible by the modulus" $ \ms -> do
        let m = ms V.! 0
        throwTarget 500 m `shouldBe` 3

  describe "throwItems" $ do
    before withMonkeys $ do
      it "throws one monkey's items and gives the updated monkeys" $ \ms -> do
        let m = ms V.! 0
        let tm = ms V.! 3
        throwItems 3 (inspectionModulus ms) ms 0
          `shouldBe` ms
            V.// [ (0, m {items = DQ.empty, nInspections = 2}),
                   (3, tm {items = DQ.fromList [74, 500, 620]})
                 ]

  describe "doRound" $ do
    before withMonkeys $ do
      it "does a round of all monkeys throwing and gives the updated monkeys" $ \ms -> do
        doRound 3 (inspectionModulus ms) ms
          `shouldBe` V.fromList
            [ (ms V.! 0) {items = DQ.fromList [20, 23, 27, 26], nInspections = 2},
              (ms V.! 1) {items = DQ.fromList [2080, 25, 167, 207, 401, 1046], nInspections = 4},
              (ms V.! 2) {items = DQ.empty, nInspections = 3},
              (ms V.! 3) {items = DQ.empty, nInspections = 5}
            ]

  describe "doRounds" $ do
    before withMonkeys $ do
      it "runs a number of rounds" $ \ms -> do
        doRounds 20 3 ms
          `shouldBe` V.fromList
            [ (ms V.! 0) {items = DQ.fromList [10, 12, 14, 26, 34], nInspections = 101},
              (ms V.! 1) {items = DQ.fromList [245, 93, 53, 199, 115], nInspections = 95},
              (ms V.! 2) {items = DQ.empty, nInspections = 7},
              (ms V.! 3) {items = DQ.empty, nInspections = 105}
            ]

  describe "monkeyBusinessLevel" $ do
    before withMonkeys $ do
      it "gets the product of the top two most inspections" $ \ms -> do
        monkeyBusinessLevel 2 (doRound 3 (inspectionModulus ms) ms) `shouldBe` 20

  describe "inspectionModulus" $ do
    before withMonkeys $ do
      it "gets the lowest common multiplier of all the monkeys' moduli" $ \ms -> do
        inspectionModulus ms `shouldBe` 96577
