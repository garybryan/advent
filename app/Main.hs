module Main (main) where

import qualified Advent2021.Day1
import qualified Advent2021.Day2
import qualified Advent2022.Day1

main :: IO ()
main = do
  putStrLn("\n2021 day 1:")
  Advent2021.Day1.run "data/2021/1.txt"

  putStrLn("\n2021 day 2:")
  Advent2021.Day2.run "data/2021/2.txt"

  putStrLn("\n2022 day 1:")
  Advent2022.Day1.run "data/2022/1.txt"
