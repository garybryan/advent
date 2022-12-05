module Advent2022.Day05.Part2 (moveSeveralAtOnce, applyMovesToLinesSeveralAtOnce, run) where

import Advent2022.Day05.Base
import qualified Data.Dequeue as DQ
import Data.Maybe
import qualified Data.Vector as V

-- Save myself some typing.
type Deque = DQ.BankersDequeue

-- TODO can probably be done with foldr/iterate for repeated function application
popBackSeveralHelper :: [a] -> Int -> Deque a -> ([a], Deque a)
popBackSeveralHelper vs 0 s = (vs, s)
popBackSeveralHelper vs n s = popBackSeveralHelper (v : vs) (n - 1) s'
  where
    (v, s') = fromMaybe (error "Empty stack!") (DQ.popBack s)

popBackSeveral :: Int -> Deque a -> ([a], Deque a)
popBackSeveral = popBackSeveralHelper []

pushBackSeveral :: Deque a -> [a] -> Deque a
pushBackSeveral = foldl DQ.pushBack

moveSeveralAtOnce :: Move -> Stacks -> Stacks
moveSeveralAtOnce (n, from, to) ss = ss V.// [(from0, newFrom), (to0, newTo)]
  where
    from0 = from - 1
    to0 = to - 1
    (cs, newFrom) = popBackSeveral n (ss V.! from0)
    newTo = pushBackSeveral (ss V.! to0) cs

applyMovesToLinesSeveralAtOnce :: [String] -> Stacks
applyMovesToLinesSeveralAtOnce = applyMovesToLines moveSeveralAtOnce

topCratesFromLines :: [String] -> String
topCratesFromLines = topCrates . applyMovesToLinesSeveralAtOnce

run :: [String] -> String
run ls = "Final top crates from moving several at a time: " ++ show (topCratesFromLines ls)
