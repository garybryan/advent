module Advent2022.Day19.Base
  ( State (..),
    Costs,
    Blueprint,
    getMaxCosts,
    canBuild,
    shouldBuild,
    buildRobot,
    bnbFromInitial,
  )
where

import Data.Maybe (mapMaybe)
import qualified Data.PQueue.Max as PQ
import qualified Data.Vector as V

{-
  Structure this as minutes, states, events, and choices...

  State: number of each type of resource and type of robot.

  Events: on each turn, each robot collects 1 of its resource type, so each
  resource amount increases by the quantity of corresponding robots.

  Choices:
    - If enough ore, can build an ore robot.
    - If enough ore, can build a clay robot.
    - If enough ore and clay, can build an obsidian robot.
    - If enough ore and obsidian, can build a geode robot.
  It seems that the factory can only construct a single type of robot per turn,
  which is good as it narrows down the search space. But that could still be up
  to 4 branches for each minute, resulting in a 4^24 graph, although it's
  unlikely that all choices will be available at all times.

  This is an optimisation problem similar to Day 16. I'm reluctant to do another
  dynamic programming solution as I ended up painting myself into a corner and it
  wasn't very reusable for Part 2, so maybe this time it's better to consider
  optimised search strategies like branch-and-bound first.

  Represent resource types as vectors, where position 0 is ore, 1 is clay, 2 is
  obsidian, 3 is geode.

  B&B idea: use a simple greedy method for a lower bound. Then do a BFS but
  don't explore any branches that will produce a lower result. When finding a
  higher result, update the lower bound to that.

  The greedy approach builds a geode robot if possible, and does nothing
  otherwise.

  This worked but was still really slow, so I added some optimistic upper
  bounding too by relaxing the constraints: assume you can build a geode robot
  at every minute. If this upper bound is still lower than the current lower
  bound, prune the branch. And I simplified the lower bound calculation to a
  constant-time one of working out the number of geodes at the end based on
  doing nothing other than continuing to produce with the current robots.

  And that was still really slow. The big win was to avoid considering every
  state at every minute and basing the logic on whether a robot can be built at
  the current minute, and instead say that you can build a robot any time you
  have enough current robots to produce the necessary resources, even if you
  don't actually have these resources yet, and calculate (in constant time) how
  long is needed to build it and what the resources will be after that time has
  passed and it has been built. This saves a huge number of unnecessary "saving
  up resources" branches and still covers the whole problem space.

  Part 2 was still quite slow, but turning the breadth-first search into a
  best-first search by changing the standard queue to a priority queue with
  priority based on resource numbers and elapsed time sorted that out. This
  makes it follow paths with more geodes in a depth-first way, establishing
  higher bounds more quickly and allowing more pruning.
-}

type Robots = V.Vector Int

type Resources = V.Vector Int

data State = State
  { robots :: Robots,
    resources :: Resources,
    minute :: Int
  }
  deriving (Eq, Ord, Show)

type Cost = V.Vector Int

type Costs = V.Vector Cost

type MaxCosts = V.Vector Int

type Blueprint = (Int, Costs)

type Priority = (Int, Int, Int, Int)

type Queue = PQ.MaxQueue (Priority, State)

canBuild :: Cost -> Robots -> Bool
canBuild robotCost rs = and $ V.zipWith canProduceOrDontNeed robotCost rs
  where
    canProduceOrDontNeed c r = c == 0 || r > 0

-- Only build enough robots to build other ones in a turn:
-- If a robot costs 5 of a resource, we don't need more than 5
-- robots producing that resource.
shouldBuild :: MaxCosts -> Int -> Robots -> Bool
shouldBuild _ 3 _ = True
shouldBuild maxCosts i rs = (rs V.! i) < maxCosts V.! i

timetoBuild :: Cost -> State -> Int
timetoBuild robotCost state = 1 + V.maximum extraResourcesRequired
  where
    costRobotsAndResources = V.zip3 robotCost (robots state) (resources state)
    extraResourcesRequired = V.map required costRobotsAndResources
    required (cost, nRobot, nResource)
      | nRobot == 0 || nResource >= cost = 0
      | otherwise = -((cost - nResource) `div` (-nRobot)) -- Good old double negation to turn floor into ceil

-- Can the robot be built so it's productive before time runs out?
-- No point in it being ready on the last minute as it won't have time to build anything.
enoughTimeToBuild :: Cost -> State -> Int -> Bool
enoughTimeToBuild cost state remainingMins = timetoBuild cost state < remainingMins

buildRobot :: Costs -> Int -> State -> State
buildRobot costs i state = State robots' resources' minute'
  where
    robotCost = costs V.! i
    timeRequired = timetoBuild robotCost state
    robots' = robots state V.// [(i, (robots state V.! i) + 1)]
    costRobotsAndResources = V.zip3 robotCost (robots state) (resources state)
    resources' = V.map updateResource costRobotsAndResources
    updateResource (cost, nRobot, nResource) = nResource + nRobot * timeRequired - cost
    minute' = minute state + timeRequired

nextStateWithBuild :: Int -> Costs -> MaxCosts -> State -> Int -> Maybe State
nextStateWithBuild limit costs maxCosts state i
  | canAndShouldBuild = Just $ buildRobot costs i state
  | otherwise = Nothing
  where
    robotCost = costs V.! i
    canAndShouldBuild =
      canBuild robotCost (robots state)
        && shouldBuild maxCosts i (robots state)
        && enoughTimeToBuild robotCost state (limit - minute state)

nextStates :: Int -> Costs -> MaxCosts -> State -> [State]
nextStates limit costs maxCosts state = mapMaybe (nextStateWithBuild limit costs maxCosts state) [3, 2, 1, 0]

nGeodes :: State -> Int
nGeodes state = resources state V.! 3

nGeodeRobots :: State -> Int
nGeodeRobots state = robots state V.! 3

initialState :: State
initialState = State (V.fromList [1, 0, 0, 0]) (V.replicate 4 0) 0

-- Lower bound: number of geodes that will be produced from doing nothing more
-- for the remaining time.
minGeodes :: Int -> State -> Int
minGeodes limit state = nGeodes state + remainingMins * nGeodeRobots state
  where
    remainingMins = limit - minute state

-- Optimistic bound: what if we could make a geode robot for every remaining
-- minute?
-- If this still ends up lower than the current bound, prune the branch.
maxGeodes :: Int -> State -> Int
maxGeodes limit state = minGeodes limit state + extraGeodes
  where
    remainingMins = limit - minute state
    extraGeodes = remainingMins * (remainingMins - 1) `div` 2

getMaxCosts :: Costs -> MaxCosts
getMaxCosts costs = V.generate 3 maxFor
  where
    maxFor i = maximum [cost V.! i | cost <- V.toList costs]

priority :: State -> Priority
priority state = (rs V.! 3, rs V.! 2, rs V.! 1, minute state)
  where
    rs = resources state

queueEntry :: State -> (Priority, State)
queueEntry state = (priority state, state)

bnbFromInitial :: Int -> Costs -> Int
bnbFromInitial limit costs = bnb limit costs maxCosts 0 (PQ.singleton $ queueEntry initialState)
  where
    maxCosts = getMaxCosts costs

bnb :: Int -> Costs -> MaxCosts -> Int -> Queue -> Int
bnb limit costs maxCosts bound q
  | PQ.null q = bound
  | otherwise = bnb limit costs maxCosts bound' q'
  where
    ((_, state), poppedQ) = PQ.deleteFindMax q
    lowerBound = minGeodes limit state
    upperBound = maxGeodes limit state
    q'
      | upperBound <= bound = poppedQ
      | otherwise = foldr (PQ.insert . queueEntry) poppedQ $ nextStates limit costs maxCosts state
    bound' = max bound lowerBound
