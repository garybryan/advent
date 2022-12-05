module Advent2022.Day05.Part1 (Stack, Stacks, move) where

import Advent2022.Day05.Base
import qualified Data.Dequeue as DQ
import Data.Maybe
import qualified Data.Vector as V

type Stack = DQ.BankersDequeue Char

type Stacks = V.Vector Stack

-- TODO with recursion and lists?
move :: Int -> Int -> Stacks -> Stacks
move from to ss = ss V.// [(from0, newFrom), (to0, newTo)]
  where
    from0 = from - 1
    to0 = to - 1
    (c, newFrom) = fromMaybe (error "Empty stack!") (DQ.popBack $ ss V.! from0)
    newTo = DQ.pushBack (ss V.! to0) c

-- run :: [String] -> String
-- run ls = "Result: " ++ show (doThing)
