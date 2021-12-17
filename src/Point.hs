module Point
  (Point,
   mooreNeighbors
  ) where

type Point = (Int, Int)

mooreNeighbors :: Point -> [Point]
mooreNeighbors (x, y) = [
  (succ x, y),
  (pred x, y),
  (x, succ y),
  (x, pred y)
  ]
