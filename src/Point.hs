module Point
  ( Point,
    mooreNeighbors,
    manhattan
  )
where

type Point = (Int, Int)

mooreNeighbors :: Point -> [Point]
mooreNeighbors (x, y) =
  [ (succ x, y),
    (pred x, y),
    (x, succ y),
    (x, pred y)
  ]

manhattan :: Point -> Int
manhattan (x, y) = abs x + abs y
