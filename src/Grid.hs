module Grid (Grid, fromLists) where

import Data.Array (Array, array)

type Grid a = Array (Int, Int) a

fromLists :: [[a]] -> Grid a
fromLists rows =
  let dims = ((0, 0), ((pred (length rows)), (pred (length (head rows)))))
      values = [((x, y), element) | (xs, x) <- zip rows [0 ..], (element, y) <- zip xs [0 ..]]
   in array dims values
