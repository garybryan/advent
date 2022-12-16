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
import qualified Advent2022.Day07.Part1
import qualified Advent2022.Day07.Part2
import qualified Advent2022.Day08.Part1
import qualified Advent2022.Day08.Part2
import qualified Advent2022.Day09.Part1
import qualified Advent2022.Day09.Part2
import qualified Advent2022.Day10.Part1
import qualified Advent2022.Day10.Part2
import qualified Advent2022.Day11.Part1
import qualified Advent2022.Day11.Part2
-- import qualified Advent2022.Day12.Part1
-- import qualified Advent2022.Day12.Part2
import qualified Advent2022.Day13.Part1
import qualified Advent2022.Day13.Part2
import qualified Advent2022.Day14.Part1
import qualified Advent2022.Day14.Part2
import Lib.Run

-- TODO implement a better run system:
-- Main could take a year and day as a parameter, and call the appropriate `run` functions.

main :: IO ()
main = do
  putStrLn "\n2021 day 1:"
  runOnFile Advent2021.Day01.run "data/2021/01.txt"

  putStrLn "\n2021 day 2:"
  runOnFile Advent2021.Day02.run "data/2021/02.txt"

  putStrLn "\n2021 day 3 part 1:"
  runOnFile Advent2021.Day03.Part1.run "data/2021/03.txt"
  putStrLn "\n2021 day 3 part 2:"
  runOnFile Advent2021.Day03.Part2.run "data/2021/03.txt"

  putStrLn "\n2022 day 1:"
  runOnFile Advent2022.Day01.run "data/2022/01.txt"

  putStrLn "\n2022 day 2 part 1:"
  runOnFile Advent2022.Day02.Part1.run "data/2022/02.txt"
  putStrLn "2022 day 2 part 2:"
  runOnFile Advent2022.Day02.Part2.run "data/2022/02.txt"

  putStrLn "\n2022 day 3 part 1:"
  runOnFile Advent2022.Day03.Part1.run "data/2022/03.txt"
  putStrLn "\n2022 day 3 part 2:"
  runOnFile Advent2022.Day03.Part2.run "data/2022/03.txt"

  putStrLn "\n2022 day 4 part 1:"
  runOnFile Advent2022.Day04.Part1.run "data/2022/04.txt"
  putStrLn "\n2022 day 4 part 2:"
  runOnFile Advent2022.Day04.Part2.run "data/2022/04.txt"

  putStrLn "\n2022 day 5 part 1:"
  runOnFile Advent2022.Day05.Part1.run "data/2022/05.txt"
  putStrLn "\n2022 day 5 part 2:"
  runOnFile Advent2022.Day05.Part2.run "data/2022/05.txt"

  putStrLn "\n2022 day 6 part 1:"
  runOnFile Advent2022.Day06.Part1.run "data/2022/06.txt"
  putStrLn "\n2022 day 6 part 2:"
  runOnFile Advent2022.Day06.Part2.run "data/2022/06.txt"

  putStrLn "\n2022 day 7 part 1:"
  runOnFile Advent2022.Day07.Part1.run "data/2022/07.txt"
  putStrLn "\n2022 day 7 part 2:"
  runOnFile Advent2022.Day07.Part2.run "data/2022/07.txt"

  putStrLn "\n2022 day 8 part 1:"
  runOnFile Advent2022.Day08.Part1.run "data/2022/08.txt"
  putStrLn "\n2022 day 8 part 2:"
  runOnFile Advent2022.Day08.Part2.run "data/2022/08.txt"

  putStrLn "\n2022 day 9 part 1:"
  runOnFile Advent2022.Day09.Part1.run "data/2022/09.txt"
  putStrLn "\n2022 day 9 part 2:"
  runOnFile Advent2022.Day09.Part2.run "data/2022/09.txt"

  putStrLn "\n2022 day 10 part 1:"
  runOnFile Advent2022.Day10.Part1.run "data/2022/10.txt"
  putStrLn "\n2022 day 10 part 2:"
  runOnFile Advent2022.Day10.Part2.run "data/2022/10.txt"

  putStrLn "\n2022 day 11 part 1:"
  runOnFile Advent2022.Day11.Part1.run "data/2022/11.txt"
  putStrLn "\n2022 day 11 part 2:"
  runOnFile Advent2022.Day11.Part2.run "data/2022/11.txt"

  -- Too slow!
  -- putStrLn "\n2022 day 12 part 1:"
  -- runOnFile Advent2022.Day12.Part1.run "data/2022/12.txt"
  -- putStrLn "\n2022 day 12 part 2:"
  -- runOnFile Advent2022.Day12.Part2.run "data/2022/12.txt"

  putStrLn "\n2022 day 13 part 1:"
  runOnFile Advent2022.Day13.Part1.run "data/2022/13.txt"
  putStrLn "\n2022 day 13 part 2:"
  runOnFile Advent2022.Day13.Part2.run "data/2022/13.txt"

  putStrLn "\n2022 day 14 part 1:"
  runOnFile Advent2022.Day14.Part1.run "data/2022/14.txt"
  putStrLn "\n2022 day 14 part 2:"
  runOnFile Advent2022.Day14.Part2.run "data/2022/14.txt"
