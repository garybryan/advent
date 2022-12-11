module Advent2022.Day08.Base
  ( TreeMatrix,
    parseLines,
  )
where

import Data.Char (digitToInt)
import qualified Data.Matrix as M

type TreeMatrix = M.Matrix Int

parseLines :: [String] -> TreeMatrix
parseLines ss = M.fromLists [map digitToInt s | s <- ss]
