module Grid
  ( Grid,
    (!?),
    (!),
    boundsInclusive,
    fromList,
    fromLists,
    generate,
    gridCol,
    gridHeight,
    gridMoores,
    gridRow,
    gridRows,
    gridVonNeumanns,
    gridWidth,
    insert,
    isInBounds,
    mapWithIndex,
    points,
    pointsWhere,
    render,
    size,
    toLists,
    values,
  )
where

import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Point (Point)

newtype Grid a = Grid (Map.Map (Int, Int) a)
  deriving (Eq)

instance Functor Grid where
  fmap fn (Grid grid) = Grid (fmap fn grid)

--
-- Constructors
--

fromLists :: [[a]] -> Grid a
fromLists rows =
  let maxX = length $ head rows
      maxY = length rows
      entries = do
        x <- [0 .. maxX - 1]
        y <- [0 .. maxY - 1]
        let e = rows !! y !! x
        return ((x, y), e)
   in Grid $ Map.fromList entries

fromList :: [(Point, a)] -> Grid a
fromList = Grid . Map.fromList

generate :: (Int, Int) -> (Point -> a) -> Grid a
generate (width, height) pointFn =
  let entries = do
        x <- [0 .. (pred width)]
        y <- [0 .. (pred height)]
        return $ ((x, y), pointFn (x, y))
   in fromList entries

--
-- Deconstructors
--

toLists :: a -> Grid a -> [[a]]
toLists aIfEmpty grid =
  let (maxX, maxY) = boundsInclusive grid
      xs = [0 .. maxX]
      ys = [0 .. maxX]
   in [[fromMaybe aIfEmpty (grid !? (x, y)) | x <- xs] | y <- ys]

values :: Grid a -> [a]
values (Grid m) = Map.elems m

--
-- Accessors
--

(!?) :: Grid a -> Point -> Maybe a
(!?) (Grid grid) p = grid Map.!? p

(!) :: Grid a -> Point -> a
(!) (Grid grid) p = grid Map.! p

size :: Grid a -> (Int, Int)
size grid =
  let ps = points grid
      maxX = maximum $ fst <$> ps
      maxY = maximum $ snd <$> ps
   in (maxX + 1, maxY + 1)

boundsInclusive :: Grid a -> (Int, Int)
boundsInclusive (Grid grid) =
  let points = Map.keys grid
   in (maximum (map fst points), maximum (map snd points))

isInBounds :: Grid a -> (Int, Int) -> Bool
isInBounds grid (x, y) =
  let (maxX, maxY) = boundsInclusive grid
   in x >= 0 && x <= maxX && y >= 0 && y <= maxY

pointsWhere :: (Maybe a -> Bool) -> Grid a -> [Point]
pointsWhere fn grid =
  let (maxX, maxY) = boundsInclusive grid
   in [(x, y) | x <- [0 .. maxX], y <- [0 .. maxY], fn (grid !? (x, y))]

gridWidth :: Grid a -> Int
gridWidth = fst . boundsInclusive

gridHeight :: Grid a -> Int
gridHeight = snd . boundsInclusive

gridRow :: Int -> Grid a -> [Maybe a]
gridRow y grid =
  let width = gridWidth grid
      indices = [(x, y) | x <- [0 .. width]]
   in map (grid !?) indices

gridRows :: Grid a -> [[Maybe a]]
gridRows grid = [gridRow y grid | y <- [0 .. (gridHeight grid)]]

gridCol :: Int -> Grid a -> [Maybe a]
gridCol x grid =
  let height = gridHeight grid
      indices = [(x, y) | y <- [0 .. height]]
   in map (grid !?) indices

points :: Grid a -> [(Int, Int)]
points grid =
  let ((maxX, maxY)) = boundsInclusive grid
   in [(x, y) | x <- [0 .. maxX], y <- [0 .. maxY]]

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

gridMooreValues :: Grid a -> Point -> [Maybe a]
gridMooreValues grid p =
  let moores = gridMoores grid p
   in map (grid !?) moores

--
-- Mutators
--

insert :: Point -> a -> Grid a -> Grid a
insert p value (Grid grid) = Grid $ Map.insert p value grid

mapWithIndex :: (Point -> a -> b) -> Grid a -> Grid b
mapWithIndex fn (Grid grid) = Grid $ Map.mapWithKey fn grid

render :: Show a => String -> Grid a -> String
render ifEmpty grid =
  let lists = toLists ifEmpty $ fmap show grid
   in intercalate "\n" $ map (intercalate "") $ lists
