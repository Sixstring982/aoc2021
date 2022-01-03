module Problem5 (problem) where

import Range (range)
import Control.Monad.Trans.Reader (asks)
import Data.List (find, intercalate, transpose)
import qualified Data.Map as Map
import Data.Map ((!?))
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Environment (Env (envInput))
import Problem (Problem)
import Text.Parsec

type Parser = Parsec String ()

---------
-- Points
---------

type Point = (Int, Int)

parsePoint :: Parser Point
parsePoint = do
  x <- many digit
  _ <- char ','
  y <- many digit
  return (read x, read y)

plotPoints :: Map.Map Point Int -> [String]
plotPoints points =
  let xs = map fst $ Map.keys points
      ys = map snd $ Map.keys points
      minX = minimum xs
      maxX = maximum xs
      minY = minimum ys
      maxY = maximum ys
      chars :: [String] =
        [ [ fromMaybe '.' (fmap (head . show) (points !? (x, y)))
            | y <- [minY .. maxY]
          ]
          | x <- [minX .. maxX]
        ]
   in chars

--------
-- Vents
--------

type Vent = (Point, Point)

parseVent :: Parser Vent
parseVent = do
  start <- parsePoint
  _ <- string " -> "
  end <- parsePoint
  _ <- endOfLine
  return (start, end)

pointsIfVertical :: Vent -> [Point]
pointsIfVertical ((x1, y1), (x2, y2))
  | x1 /= x2 = []
  | otherwise =
    let minY = min y1 y2
        maxY = max y1 y2
     in [(x1, y) | y <- [minY .. maxY]]

pointsIfHorizontal :: Vent -> [Point]
pointsIfHorizontal ((x1, y1), (x2, y2))
  | y1 /= y2 = []
  | otherwise =
    let minX = min x1 x2
        maxX = max x1 x2
     in [(x, y1) | x <- [minX .. maxX]]

pointsIfDiagonal :: Vent -> [Point]
pointsIfDiagonal ((x1, y1), (x2, y2))
  | abs (x1 - x2) /= abs (y1 - y2) = []
  | otherwise = zip (range x1 x2) (range y1 y2)

pointsForVent :: Vent -> [Point]
pointsForVent v =
  pointsIfVertical v ++ pointsIfHorizontal v ++ pointsIfDiagonal v

countPoints :: [Point] -> Map.Map Point Int
countPoints [] = Map.empty
countPoints (p : ps) =
  let otherPoints = countPoints ps
   in Map.insertWith (+) p 1 otherPoints

intersection :: Vent -> Vent -> Set.Set Point
intersection v1 v2 =
  let p1 = Set.fromList $ pointsForVent v1
      p2 = Set.fromList $ pointsForVent v2
   in Set.intersection p1 p2

allOverlaps :: [Vent] -> Set.Set Point
allOverlaps vents =
  let intersections = [intersection v1 v2 | v1 <- vents, v2 <- vents, v1 /= v2]
   in foldl1 Set.union intersections

----------------
-- Input parsing
----------------

parseVentFile :: Parser [Vent]
parseVentFile = do
  vents <- many parseVent
  eof
  return vents

----------
-- Problem
----------

problem :: Problem
problem = do
  input <- asks envInput
  let (Right vents) = parse parseVentFile "" input
  return $ show $ length $ Set.toList $ allOverlaps vents
