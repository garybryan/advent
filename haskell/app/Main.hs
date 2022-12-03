module Main (main) where

import qualified Advent2021.Day1
import qualified Advent2021.Day2
import qualified Advent2021.Day3.Part1
import qualified Advent2022.Day1
import qualified Advent2022.Day2.Part1
import qualified Advent2022.Day2.Part2
import qualified Advent2022.Day3.Part1
import qualified Advent2022.Day3.Part2

-- TODO implement a better run system:
-- Main could take a year and day as a parameter, and call the appropriate `run` functions.

main :: IO ()
main = do
  putStrLn "\n2021 day 1:"
  Advent2021.Day1.run "data/2021/1.txt"

  putStrLn "\n2021 day 2:"
  Advent2021.Day2.run "data/2021/2.txt"

  putStrLn "\n2021 day 3 part 1:"
  Advent2021.Day3.Part1.run "data/2021/3.txt"

  putStrLn "\n2022 day 1:"
  Advent2022.Day1.run "data/2022/1.txt"

  putStrLn "\n2022 day 2 part 1:"
  Advent2022.Day2.Part1.run "data/2022/2.txt"
  putStrLn "2022 day 2 part 2:"
  Advent2022.Day2.Part2.run "data/2022/2.txt"

  putStrLn "\n2022 day 3 part 1:"
  Advent2022.Day3.Part1.run "data/2022/3.txt"
  putStrLn "\n2022 day 3 part 2:"
  Advent2022.Day3.Part2.run "data/2022/3.txt"
