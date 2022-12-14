module Lib.Matrix
  ( matrixFoldr,
  )
where

import qualified Data.Matrix as M

-- Data.Matrix doesn't implement matrix folding, so I guess I have to do it.
matrixFoldr :: (a -> M.Matrix b -> M.Matrix b) -> M.Matrix b -> [a] -> M.Matrix b
matrixFoldr _ z [] = z
matrixFoldr f z (m : ms) = f m (matrixFoldr f z ms)
