module Advent2022.Day16.Part2
  ( doThing,
    run,
  )
where

import Advent2022.Day16.Base

{-
  Idea:
  Process each row twice. The elephant always makes a move after the player, so
  after the optimal player choices have been made, the elephant can take its
  turn. This will result in the row being updated.

  Just need to understand how to update it for the elephant. It will need to
  take into account what the player has done: pressure increases, rate
  increases, valve opening.

  Or have two rows for each minute: player row and elephant row. Would that
  make it more or less complicated?

  Or do we just run it for 52 turns rather than 30, but only increment pressure
  every second time?

  Can we consider the two best moves for each row, after filling it in, and
  work with both of these somehow? Adding their pressures together won't make sense,
  since there will be overlaps, but could we get the union of opened valves?
-}

doThing :: String
doThing = "Did a thing, part 2."

run :: [String] -> String
run ls = "Result: " ++ show (doThing)
