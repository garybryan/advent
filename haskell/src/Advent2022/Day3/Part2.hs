module Advent2022.Day3.Part2 (priorityGroupsOf3, run) where

import Advent2022.Day3.Base
import Lib.Read (readLines)

groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf k l = take k l : groupsOf k (drop k l)

priorityGroupsOf3 :: [String] -> Int
priorityGroupsOf3 = sum . map (priority . commonItemAll) . groupsOf 3

run :: FilePath -> IO ()
run filePath = do
  fileLines <- readLines filePath

  let priority3s = priorityGroupsOf3 fileLines
  putStrLn $ "Total priority of common items in groups of 3: " ++ show priority3s
