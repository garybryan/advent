module Advent2022.Day16.Part2
  ( doThing,
    run,
  )
where

import Advent2022.Day16.Base

{-
  Idea:
  Process each row twice. The elephant always make a move after the player,
  so after the optimal player choices have been made, the elephant can take its turn.
  This will result in the row being updated.

  Just need to understand how to update it for the elephant. It will need to take into
  account what the player has done: pressure increases, rate increases, valve opening.
-}

doThing :: String
doThing = "Did a thing, part 2."

run :: [String] -> String
run ls = "Result: " ++ show (doThing)
