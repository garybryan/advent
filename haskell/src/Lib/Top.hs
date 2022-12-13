module Lib.Top (topK) where

import qualified Data.Heap as H
import Data.Maybe (fromMaybe)

type IntHeap = H.MinHeap Int

-- Standard k-max elements implementation using a min-heap to store the k largest values.
-- Sorting the list and returning the top k values is O(n log n), but this is O(n log k)
-- since it only stores k elements on the heap, and even better in the average case of a
-- non-ascending list since heap operations are only done when needed.
topK :: Int -> [Int] -> [Int]
topK k xs = H.toList $ foldl updateHeap initialHeap rest
  where
    initialHeap = H.fromList (take k xs) :: IntHeap
    rest = drop k xs
    updateHeap h x
      | x > fromMaybe (error "Empty heap") (H.viewHead h) = H.insert x (H.drop 1 h)
      | otherwise = h
