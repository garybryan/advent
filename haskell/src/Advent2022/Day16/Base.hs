module Advent2022.Day16.Base
  ( ValveName,
    NamedValve,
    Valve (..),
    Valves,
    ValveIndex,
    BitVector,
    initialDistances,
    shortestPaths,
    maxPressureAndPressures,
    valvesTotalRate,
    Maxes,
  )
where

import Data.Bits
import qualified Data.Map as Map
import qualified Data.Matrix as M
import Data.Maybe (mapMaybe)
import qualified Data.PQueue.Max as PQ
import qualified Data.Set as Set
import qualified Data.Vector as V
import Data.Word (Word64)

{-
  I had a dynamic programming solution for this after finding plain
  backtracking too slow, which was pretty fast and gave a correct answer for
  part 1 but it was also hard to work with and was giving wrong answers for
  part 2.

  Inspired by day 19, I've redone it as branch-and-bound best-first search.

  The first implementation considered the choices at each state to be moving to
  any adjacent valve or opening any unopened non-zero adjacent valve. This
  worked, but took around 30 seconds to find the solution for the real data.

  To improve this, I returned to an idea I had before the DP solution but
  didn't explore: pre-calculating all the distances between valves. This turns
  the problem into simply choosing which order to open the valves in, with
  the required times (distances) to open each valve from any other valve being
  pre-calculated, so like with day 19 we can "fast-forward" in time and avoid
  all the intermediary states. So a bit of extra polynomial work at the start
  saves us a lot of non-polynomial work in the main part. This now runs in
  < 0.2 seconds.

  A matrix of the shortest paths between all node pairs is built using the
  Floyd-Warshall algorithm, which uses dynamic programming to incrementally
  consider paths between vertices via other vertices to find the minimum such
  paths. It could also be done by doing a BFS from each start node, but it's
  less elegant and possibly slower if there are many edges.
-}

type ValveName = String

type ValveIndex = Int

type AdjList = [ValveIndex]

type Distances = M.Matrix Int

type NamedValve = (ValveName, (Int, [ValveName]))

data Valve = Valve
  { valveRate :: Int,
    valveAdj :: AdjList
  }
  deriving (Eq, Show)

type Valves = V.Vector Valve

type Priority = Int

type Queue = PQ.MaxQueue (Priority, State)

-- The supplied data has 57 valves, so 64 bits just about covers us.
type BitVector = Word64

data State = State
  { valveIndex :: Int,
    rate :: Int,
    pressure :: Int,
    timeRemaining :: Int,
    openedValves :: BitVector
  }
  deriving (Eq, Ord, Show)

type StateSet = Set.Set State

type Maxes = Map.Map BitVector Int

-- This does repeated scans of adjacency lists with `elem` when building the
-- matrix, which could be avoided by using sets or adjacency matrix etc. but
-- it's a drop in the ocean for overall complexity.
initialDistances :: Valves -> Distances
initialDistances valves = M.matrix n n oneIfAdjacent
  where
    n = V.length valves
    oneIfAdjacent (row, col)
      | row == col = 0
      | (col - 1) `elem` valveAdj (valves V.! (row - 1)) = 1
      | otherwise = -1

floydWarshall :: Distances -> Int -> Int -> Int -> Distances
floydWarshall dists k i j
  | k > n = dists
  | i > n = floydWarshall dists (k + 1) 1 1
  | j > n = floydWarshall dists k (i + 1) 1
  | otherwise = floydWarshall dists' k i (j + 1)
  where
    n = M.nrows dists
    distIJ = dists M.! (i, j)
    distIK = dists M.! (i, k)
    distKJ = dists M.! (k, j)
    distViaK = dists M.! (i, k) + dists M.! (k, j)
    dists'
      | distIK == -1 || distKJ == -1 = dists
      | distIJ == -1 || distViaK < distIJ = M.setElem distViaK (i, j) dists
      | otherwise = dists

shortestPaths :: Valves -> Distances
shortestPaths valves = floydWarshall initialDists 1 1 1
  where
    initialDists = initialDistances valves

lowerBound :: State -> Int
lowerBound state = pressure state + rate state * timeRemaining state

{-
 Optimistic bound: imagine we could open all remaining valves next.
 The total rate (all valves open) has been pre-calculated, so the final
 pressure would be the min bound + rate for all unopened valves for 2 minutes
 later and onwards. Since stateChoices prunes any states before the last two
 minutes, there's no risk of funkiness from negative numbers here.
-}
optBound :: Int -> State -> Int
optBound totalRate state =
  lowerBound state + (totalRate - rate state) * (timeRemaining state - 2)

priority :: State -> Priority
priority = lowerBound

queueEntry :: State -> (Priority, State)
queueEntry state = (priority state, state)

-- O(V) function to get the indices of openable (not opened and rate > 0)
-- valves from a bit vector. Could be improved by using IntSets, but bit
-- vectors should be a big advantage for comparing sets in part 2...
openableValveIndices :: BitVector -> Valves -> [Int]
openableValveIndices opened = map fst . V.toList . V.filter snd . V.imap openableAndNotOpened
  where
    openableAndNotOpened i v = (i, valveRate v > 0 && not (opened `testBit` i))

{-
  Get the state for moving to and opening a valve from the current state.

  Don't bother opening a valve if the time remaining isn't sufficient to get to
  it (1 minute per distance unit), open it (1 minute), and benefit from the
  pressure release. Gives Nothing if there's not enough time.
-}
choice :: Valves -> Distances -> State -> Int -> Maybe State
choice valves distances state openIndex
  | timeToOpen < remaining =
      Just
        State
          { valveIndex = openIndex,
            timeRemaining = remaining - timeToOpen,
            pressure = pressure state + timeToOpen * rate state,
            rate = rate state + valveRate (valves V.! openIndex),
            openedValves = openedValves state `setBit` openIndex
          }
  | otherwise = Nothing
  where
    remaining = timeRemaining state
    curIndex = valveIndex state
    timeToOpen = 1 + distances M.! (curIndex + 1, openIndex + 1)

stateChoices :: Valves -> Distances -> State -> [State]
stateChoices valves distances state = mapMaybe (choice valves distances state) openableIndices
  where
    openableIndices = openableValveIndices (openedValves state) valves

bnb :: Int -> Valves -> Distances -> Bool -> Int -> Int -> StateSet -> Queue -> Maxes -> (Int, Maxes)
bnb limit valves distances useUpperBound totalRate bound seen q maxes
  | PQ.null q = (bound, maxes)
  | otherwise = bnb limit valves distances useUpperBound totalRate bound' seen' q' maxes'
  where
    ((_, state), poppedQ) = PQ.deleteFindMax q
    unseenAndInBound s = s `Set.notMember` seen && (not useUpperBound || optBound totalRate s > bound)
    choices = filter unseenAndInBound $ stateChoices valves distances state
    q' = foldr (PQ.insert . queueEntry) poppedQ choices
    lb = lowerBound state
    bound' = max bound lb
    seen' = foldr Set.insert seen choices
    maxes' = Map.insertWith max (openedValves state) lb maxes

maxPressureAndPressures :: Int -> Valves -> Bool -> Int -> (Int, Maxes)
maxPressureAndPressures limit valves useUpperBound startValveIndex =
  bnb limit valves distances useUpperBound totalRate 0 initialSeen initialQ Map.empty
  where
    initialState = State startValveIndex 0 0 limit 0
    initialQ = PQ.singleton $ queueEntry initialState
    initialSeen = Set.singleton initialState
    distances = shortestPaths valves
    totalRate = valvesTotalRate valves

valvesTotalRate :: Valves -> Int
valvesTotalRate = sum . V.map valveRate
