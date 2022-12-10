module Advent2022.Day07.Part2
  ( unusedSpace,
    spaceToFree,
    dirsToMakeSpace,
    smallestDirToMakeSpace,
    run,
  )
where

import Advent2022.Day07.Base

unusedSpace :: FS -> Int
unusedSpace fs = 70000000 - nodeSize (fsMap fs) 0

spaceToFree :: FS -> Int
spaceToFree fs = max 0 (30000000 - unusedSpace fs)

dirsToMakeSpace :: FS -> [(String, Int)]
dirsToMakeSpace fs = dirsWithSize (>= spaceToFree fs) (fsMap fs) 0

smallestDirToMakeSpace :: FS -> Int
smallestDirToMakeSpace = minimum . map snd . dirsToMakeSpace

run :: [String] -> String
run ss = "Smallest dir to make space for update: " ++ show (smallestDirToMakeSpace fs)
  where
    fs = fsFromCommands [1 ..] (parseLines ss)
