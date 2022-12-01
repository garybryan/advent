module Advent2021.Day2 (main) where

import Lib.Read

type Command = (String, Int)
type Vector = (Int, Int)

parseLine :: String -> Command
parseLine line = (head splitLine, read (last splitLine))
  where splitLine = words line

getCommandVector :: Command -> Vector
getCommandVector ("up", n)      = (-n, 0)
getCommandVector ("down", n)    = (n, 0)
getCommandVector ("forward", n) = (0, n)
getCommandVector (_, _)         = (0, 0)

addVectors :: Vector -> Vector -> Vector
addVectors a b = (fst a + fst b, snd a + snd b)

getFinalPosition :: [Command] -> Vector
getFinalPosition cs = foldl addVectors (0, 0) (map getCommandVector cs)

getArea :: Vector -> Int
getArea (x, y) = x * y

getFinalPositionAim :: [Command] -> Vector
getFinalPositionAim cs = aux (0, 0) 0 (map getCommandVector cs)
  where
    aux v _ []                 = v
    aux (x, y) d ((dd, n):cvs) = aux (x + n, y + d' * n) d' cvs
      where d' = d + dd

main :: FilePath -> IO ()
main filePath = do
  fileLines <- readLines filePath
  let commands = map parseLine fileLines

  let result = getArea (getFinalPosition commands)
  putStrLn ("Part 1 result: " ++ show result)

  let resultAim = getArea (getFinalPositionAim commands)
  putStrLn ("Part 2 (with aim) result: " ++ show resultAim)
