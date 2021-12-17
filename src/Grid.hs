module Grid
  ( Grid,
    fromLists,
    isInBounds,
    gridWidth,
    gridHeight,
    gridRow,
    gridRows,
    gridCol,
    points
  ) where

import Data.Array ((!), Array, array, bounds)

type Grid a = Array (Int, Int) a

--
-- Constructors
--

fromLists :: [[a]] -> Grid a
fromLists rows =
  let dims = ((0, 0), ((pred (length (head rows))), (pred (length rows))))
      values = [((x, y), element) | (xs, y) <- zip rows [0 ..], (element, x) <- zip xs [0 ..]]
   in array dims values

--
-- Accessors
--

isInBounds :: Grid a -> (Int, Int) -> Bool
isInBounds grid (x, y) =
  let ((minX, minY), (maxX, maxY)) = bounds grid
   in x >= minX && x <= maxX && y >= minY && y <= maxY

gridWidth :: Grid a -> Int
gridWidth grid =
  let ((0, 0), (width, _)) = bounds grid
   in width

gridHeight :: Grid a -> Int
gridHeight grid =
  let ((0, 0), (_, height)) = bounds grid
   in height

gridRow :: Int -> Grid a -> [a]
gridRow y grid =
  let width = gridWidth grid
      indices = [(x, y) | x <- [0..width]]
   in map (grid !) indices

gridRows :: Grid a -> [[a]]
gridRows grid = [gridRow y grid | y <- [0..(gridHeight grid)]]

gridCol :: Int -> Grid a -> [a]
gridCol x grid =
  let height = gridHeight grid
      indices = [(x, y) | y <- [0..height]]
   in map (grid !) indices

points :: Grid a -> [(Int, Int)]
points grid =
  let ((minX, minY), (maxX, maxY)) = bounds grid
   in [(x, y) | x <- [minX..maxX], y <- [minY..maxY]]
