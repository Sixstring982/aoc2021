module Grid
  ( Grid,
    fromLists,
    toLists,
    isInBounds,
    findPointsWhere,
    gridWidth,
    gridHeight,
    gridRow,
    gridRows,
    gridCol,
    gridMoores,
    gridVonNeumanns,
    points,
    mapWithIndex,
    mapWithMooreValues,
  )
where

import Data.Array ((!), Array, array, assocs, bounds)
import Point (Point)

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
-- Deconstructors
--

toLists :: Grid a -> [[a]]
toLists g =
  let ((minX, minY), (maxX, maxY)) = bounds g
   in [[g ! (x, y) | x <- [minX .. maxX]] | y <- [minY .. maxY]]

--
-- Accessors
--

isInBounds :: Grid a -> (Int, Int) -> Bool
isInBounds grid (x, y) =
  let ((minX, minY), (maxX, maxY)) = bounds grid
   in x >= minX && x <= maxX && y >= minY && y <= maxY

findPointsWhere :: (a -> Bool) -> Grid a -> [Point]
findPointsWhere fn grid =
  let ((minX, minY), (maxX, maxY)) = bounds grid
   in [(x, y) | x <- [minX .. maxX], y <- [minY .. maxY], fn (grid ! (x, y))]

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
      indices = [(x, y) | x <- [0 .. width]]
   in map (grid !) indices

gridRows :: Grid a -> [[a]]
gridRows grid = [gridRow y grid | y <- [0 .. (gridHeight grid)]]

gridCol :: Int -> Grid a -> [a]
gridCol x grid =
  let height = gridHeight grid
      indices = [(x, y) | y <- [0 .. height]]
   in map (grid !) indices

points :: Grid a -> [(Int, Int)]
points grid =
  let ((minX, minY), (maxX, maxY)) = bounds grid
   in [(x, y) | x <- [minX .. maxX], y <- [minY .. maxY]]

gridVonNeumanns :: Grid a -> Point -> [Point]
gridVonNeumanns grid (x, y) =
    filter
      (isInBounds grid)
      [ (x, succ y),
        (pred x, y),
        (succ x, y),
        (x, pred y)
      ]

gridMoores :: Grid a -> Point -> [Point]
gridMoores grid (x, y) =
  filter
    (isInBounds grid)
    [ (pred x, succ y),
      (x, succ y),
      (succ x, succ y),
      (pred x, y),
      (succ x, y),
      (pred x, pred y),
      (x, pred y),
      (succ x, pred y)
    ]

gridMooreValues :: Grid a -> Point -> [a]
gridMooreValues grid p =
  let moores = gridMoores grid p
   in map (grid !) moores

--
-- Mutators
--

mapWithIndex :: (Point -> a -> b) -> Grid a -> Grid b
mapWithIndex fn grid =
  array (bounds grid) (map (\(index, a) -> (index, fn index a)) (assocs grid))

mapWithMooreValues :: ([a] -> a -> b) -> Grid a -> Grid b
mapWithMooreValues fn grid =
  array (bounds grid) (map (\(index, a) -> (index, fn (gridMooreValues grid index) a)) (assocs grid))
