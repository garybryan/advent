module Main (main) where

import qualified Advent2021.Day01
import qualified Advent2021.Day02
import qualified Advent2021.Day03.Part1
import qualified Advent2021.Day03.Part2
import qualified Advent2022.Day01
import qualified Advent2022.Day02.Part1
import qualified Advent2022.Day02.Part2
import qualified Advent2022.Day03.Part1
import qualified Advent2022.Day03.Part2
import qualified Advent2022.Day04.Part1
import qualified Advent2022.Day04.Part2
import qualified Advent2022.Day05.Part1
import qualified Advent2022.Day05.Part2
import qualified Advent2022.Day06.Part1
import qualified Advent2022.Day06.Part2
import Lib.Run

-- TODO implement a better run system:
-- Main could take a year and day as a parameter, and call the appropriate `run` functions.

main :: IO ()
main = do
  putStrLn "\n2021 day 1:"
  runOnFile Advent2021.Day01.run "data/2021/1.txt"

  putStrLn "\n2021 day 2:"
  runOnFile Advent2021.Day02.run "data/2021/2.txt"

  putStrLn "\n2021 day 3 part 1:"
  runOnFile Advent2021.Day03.Part1.run "data/2021/3.txt"
  putStrLn "\n2021 day 3 part 2:"
  runOnFile Advent2021.Day03.Part2.run "data/2021/3.txt"

  putStrLn "\n2022 day 1:"
  runOnFile Advent2022.Day01.run "data/2022/1.txt"

  putStrLn "\n2022 day 2 part 1:"
  runOnFile Advent2022.Day02.Part1.run "data/2022/2.txt"
  putStrLn "2022 day 2 part 2:"
  runOnFile Advent2022.Day02.Part2.run "data/2022/2.txt"

  putStrLn "\n2022 day 3 part 1:"
  runOnFile Advent2022.Day03.Part1.run "data/2022/3.txt"
  putStrLn "\n2022 day 3 part 2:"
  runOnFile Advent2022.Day03.Part2.run "data/2022/3.txt"

  putStrLn "\n2022 day 4 part 1:"
  runOnFile Advent2022.Day04.Part1.run "data/2022/4.txt"
  putStrLn "\n2022 day 4 part 2:"
  runOnFile Advent2022.Day04.Part2.run "data/2022/4.txt"

  putStrLn "\n2022 day 5 part 1:"
  runOnFile Advent2022.Day05.Part1.run "data/2022/05.txt"
  putStrLn "\n2022 day 5 part 2:"
  runOnFile Advent2022.Day05.Part2.run "data/2022/05.txt"

  putStrLn "\n2022 day 6 part 1:"
  runOnFile Advent2022.Day06.Part1.run "data/2022/06.txt"
  putStrLn "\n2022 day 6 part 2:"
  runOnFile Advent2022.Day06.Part2.run "data/2022/06.txt"
