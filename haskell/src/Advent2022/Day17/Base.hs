module Advent2022.Day17.Base
  ( parseLine,
    JetMove (..),
    blocks,
    jetPush,
    isOnSettled,
    fallBlock,
    fallBlocks,
    heightAfterBlocks,
  )
where

import Data.Bifunctor (bimap, first, second)
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import Lib.Types (Point)

data JetMove = JetLeft | JetRight deriving (Eq, Ord, Show)

parseChar :: Char -> JetMove
parseChar '<' = JetLeft
parseChar '>' = JetRight
parseChar c = error $ "Invalid character: " ++ show c

parseLine :: String -> [JetMove]
parseLine = map parseChar

{-
  Let's think this through before writing any code.

  We're not told it explicitly, but from the example, the next rock always
  starts falling with its bottom 4 units above the current highest point.

  So we have a coordinate system where the floor is at y = 0, the first rock
  starts falling with its leftmost point at x = 3 and bottommost point at y = 3.

  A block can be represented as a list of its coordinates relative to the
  origin, as if it were sitting at the left of the floor.

  The fallen blocks can be represented as a container of coordinates of spaces
  containing rock.

  Model a block falling by finding the maximum height 'h' of the current fallen
  blocks, and then adding (2, h) to each block coordinate. Then repeat the
  process of a jet push and a fall, and after each fall, check if the block now
  needs to settle by checking if any of its points are now one above an
  occupied point. Once a fall finishes, add all the final points to the fallen
  block container.

  The most interesting fallen blocks (occupied points) are the topmost ones: we
  need the maximum height of /any/ occupied point to determine the starting
  point of the next and to find the final height. To check occupied points
  during falling, we need to check which points are occupied for a given y
  value, ideally without having to check /every/ point.

  The structures that come to mind are a heap, or a map of y-coordinates to
  sets of x-coordinates. I prefer the latter as it allows fast checks for any
  given y value, while the heap just makes it quick to check the very top. with
  this latter structure, we can find the highest y point in O (log n) time and
  also check a point for settling in O (log n) time (looking up the y coord in
  the map plus looking up the x coord in the corresponding set).

  Checking for settling only needs to be done for the bottommost points of a
  block, which could be pre-computed, but that seems like more hassle than
  it's worth for now.
-}

-- TODO are they called rocks, not blocks?
type Block = [Point]

type XSet = IntSet.IntSet

type Settled = Map.Map Int XSet

blocks :: [Block]
blocks =
  [ [(1, 1), (2, 1), (3, 1), (4, 1)], -- horizontal bar
    [(2, 1), (1, 2), (2, 2), (3, 2), (2, 3)], -- plus shape
    [(1, 1), (2, 1), (3, 1), (3, 2), (3, 3)], -- backwards L shape
    [(1, 1), (1, 2), (1, 3), (1, 4)], -- vertical bar
    [(1, 1), (1, 2), (2, 1), (2, 2)] -- square
  ]

jetMoveN :: JetMove -> Int
jetMoveN JetLeft = -1
jetMoveN JetRight = 1

isPointSettled :: Settled -> Point -> Bool
isPointSettled s (x, y) = x `IntSet.member` Map.findWithDefault IntSet.empty y s

isOnSettled :: Settled -> Block -> Bool
isOnSettled s = any $ isPointSettled s

isOnFloor :: Block -> Bool
isOnFloor = any ((== 0) . snd)

isOnEdge :: Block -> Bool
isOnEdge = any ((\x -> x < 1 || x > 7) . fst)

jetPush :: Settled -> Block -> JetMove -> Block
jetPush s b jm
  | isOnEdge b' || isOnSettled s b' = b
  | otherwise = b'
  where
    b' = map (first $ (+) (jetMoveN jm)) b

-- Add a block's current position to the settled map.
settle :: Settled -> Block -> Settled
settle = foldr insertPoint
  where
    insertPoint (x, y) = Map.insertWith IntSet.union y (IntSet.singleton x)

fallOne :: Block -> Block
fallOne = map (second (subtract 1))

maxY :: Settled -> Int
maxY = fst . Map.findMax

start :: Settled -> Block -> Block
start s = map (bimap (+ 2) ((+ 3) . (+ topY)))
  where
    topY
      | Map.null s = 0
      | otherwise = maxY s

-- This needs to return the remaining JetMoves so that they can continue for
-- the next block after where they stopped for this one.
fallBlock :: Settled -> Block -> [JetMove] -> ([JetMove], Settled)
fallBlock origS origB = fall origS (start origS origB)
  where
    fall :: Settled -> Block -> [JetMove] -> ([JetMove], Settled)
    fall _ _ [] = error "Ran out of jet moves"
    fall s b (jm : jms)
      | isOnFloor bPushedAndFallen || isOnSettled s bPushedAndFallen = (jms, settle s bPushed)
      | otherwise = fall s bPushedAndFallen jms
      where
        bPushed = jetPush s b jm
        bPushedAndFallen = fallOne bPushed

fallBlocks :: [Block] -> [JetMove] -> Settled
fallBlocks = fall Map.empty
  where
    fall :: Settled -> [Block] -> [JetMove] -> Settled
    fall s [] _ = s
    fall s (b : bs) jetMoves = fall s' bs jetMoves'
      where
        (jetMoves', s') = fallBlock s b jetMoves

heightAfterBlocks :: Int -> [JetMove] -> Int
heightAfterBlocks n jetMoves = maxY $ fallBlocks (take n $ cycle blocks) (cycle jetMoves)
