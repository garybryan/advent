module Main (main) where

import qualified Advent2023.Day01.Part1
import Lib.Run

-- TODO implement a better run system:
-- Main could take a year and day as a parameter, and call the appropriate `run` functions.

main :: IO ()
main = do
  putStrLn "\n2023 day 1:"
  runOnFile Advent2023.Day01.Part1.run "data/2023/01.txt"
