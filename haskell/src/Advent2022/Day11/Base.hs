module Advent2022.Day11.Base
  ( Monkey (..),
    Monkeys,
    parseItemsLine,
    parseOperationLine,
    parseModulusLine,
    parseThrowLine,
    linesToMonkeys,
    inspectItem,
    throwTarget,
    throwItems,
    doRound,
    doRounds,
    monkeyBusinessLevel,
    inspectionModulus,
  )
where

import qualified Data.Dequeue as DQ
import Data.List.Split (splitOn, splitWhen)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import Lib.Top (topK)

{-
  Store monkeys in a Vector so they can be accessed from 0 to n-1.

  Given the input size, it's probably quicker to manually define the monkeys
  from the data rather than implement parsing. But doing the parsing is a good
  exercise.
-}

-- Each monkey's items actually represent a queue, since they're always taken
-- off the end and added at the start. So use a real queue! This avoids the
-- O(n) operation of appending to the end of a list.
type Items = DQ.BankersDequeue Int

data Monkey = Monkey
  { items :: Items,
    operation :: Int -> Int,
    modulus :: Int,
    ifTrue :: Int,
    ifFalse :: Int,
    nInspections :: Int
  }

{- Haskell can't `show` or compare a function, so `operation` causes an error
  if we try to derive `Show` or use it in a `shouldBe` test assertion, which
  appears to use `show` to compare. So just ignore `operation` when comparing.
  Note that this means tests comparing two monkeys will say they are equal even if
  the operations differ!
-}
instance Eq Monkey where
  m1 == m2 =
    items m1 == items m2
      && modulus m1 == modulus m2
      && ifTrue m1 == ifTrue m2
      && ifFalse m1 == ifFalse m2
      && nInspections m1 == nInspections m2

instance Show Monkey where
  show m =
    show (items m)
      +.+ show (modulus m)
      +.+ show (ifTrue m)
      +.+ show (ifFalse m)
      +.+ show (nInspections m)
    where
      (+.+) s1 s2 = s1 ++ "," ++ s2 -- Would like to use +,+ but commas aren't allowed!

type Monkeys = V.Vector Monkey

afterSubstr :: String -> String -> String
afterSubstr substr = last . splitOn substr

parseItemsLine :: String -> Items
parseItemsLine = DQ.fromList . map read . splitOn ", " . afterSubstr ": "

parseOperation :: (String, String) -> (Int -> Int)
parseOperation ("+", s) = (+) $ read s
parseOperation ("*", " old") = (^ (2 :: Int))
parseOperation ("*", s) = (*) $ read s
parseOperation o = error $ "Invalid operation " ++ show o

parseOperationLine :: String -> (Int -> Int)
parseOperationLine = parseOperation . splitAt 1 . afterSubstr "= old "

parseModulusLine :: String -> Int
parseModulusLine = read . afterSubstr "by "

parseThrowLine :: String -> Int
parseThrowLine = read . afterSubstr "monkey "

parseMonkey :: [String] -> Monkey
parseMonkey ss =
  Monkey
    { items = parseItemsLine $ lv V.! 0,
      operation = parseOperationLine $ lv V.! 1,
      modulus = parseModulusLine $ lv V.! 2,
      ifTrue = parseThrowLine $ lv V.! 3,
      ifFalse = parseThrowLine $ lv V.! 4,
      nInspections = 0
    }
  where
    lv = V.fromList ss

splitMonkeyTexts :: [String] -> [[String]]
splitMonkeyTexts = map tail . splitWhen (== "") -- We don't care about the "Monkey x:" lines.

linesToMonkeys :: [String] -> Monkeys
linesToMonkeys = V.fromList . map parseMonkey . splitMonkeyTexts

inspectItem :: Int -> Monkey -> Int -> Int -> Int
inspectItem i m d insM = (operation m i `div` d) `mod` insM

throwTarget :: Int -> Monkey -> Int
throwTarget i m = targetF m
  where
    isDivisible = i `mod` modulus m == 0
    targetF = if isDivisible then ifTrue else ifFalse

-- Throw all of the monkey at index idx's items and give the updated monkeys.
throwItems :: Int -> Int -> Monkeys -> Int -> Monkeys
throwItems divisor insM ms idx
  | items m == DQ.empty = ms
  | otherwise = throwItems divisor insM ms' idx
  where
    m = ms V.! idx
    (i, is) = fromMaybe (error "the empty check has failed...") $ DQ.popFront (items m)
    i' = inspectItem i m divisor insM
    targetIdx = throwTarget i' m
    m' = m {items = is, nInspections = nInspections m + 1}
    tm = ms V.! targetIdx
    tm' = tm {items = DQ.pushBack (items tm) i'}
    ms' = ms V.// [(idx, m'), (targetIdx, tm')]

doRound :: Int -> Int -> Monkeys -> Monkeys
doRound divisor insM ms = foldl (throwItems divisor insM) ms [0 .. V.length ms - 1]

doRounds :: Int -> Int -> Monkeys -> Monkeys
doRounds n divisor ms = iterate (doRound divisor insM) ms !! n
  where
    insM = inspectionModulus ms

mostInspections :: Int -> Monkeys -> [Int]
mostInspections n ms = topK n (map nInspections (V.toList ms))

monkeyBusinessLevel :: Int -> Monkeys -> Int
monkeyBusinessLevel n ms = product $ mostInspections n ms

{-
  Trying to run 10,000 rounds caused an integer overflow, even for 64 bits.

  Trying to do it with Integers (arbitrary-precision) rather than Ints killed
  my computer.

  We can work smarter rather than harder!

  We don't care about the final worry levels, only the final number of
  inspections, and the throws are based on divisibility by a monkey's modulus.

  There's a hint to "find another way to keep your worry levels manageable."

  If, before throwing an item, we reduce it to one that will still work with
  all of the divisibility checks, we keep the numbers manageable. So we find
  the lowest common multiplier of all the moduli and use that as a modulus
  for the inspection calculations.
-}
inspectionModulus :: Monkeys -> Int
inspectionModulus = foldr1 lcm . map modulus . V.toList
