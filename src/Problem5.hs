module Problem5 (problem, inputPath) where

import qualified Data.Set as Set
import Data.List (intercalate)
import Control.Monad.Trans.Reader (asks)
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

plotPoints :: [Point] -> String
plotPoints points =
  let xs = map fst points
      ys = map snd points
      minX = minimum xs
      maxX = maximum xs
      minY = minimum ys
      maxY = maximum ys
      chars = [[ if (x, y) `elem` points then '#' else '.' | y <- [minY..maxY]] | x <- [minX..maxX]]
   in intercalate "\n" chars

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
  | otherwise = [(x1, y) | y <- [y1..y2]]

pointsIfHorizontal :: Vent -> [Point]
pointsIfHorizontal ((x1, y1), (x2, y2))
  | y1 /= y2 = []
  | otherwise = [(x, y1) | x <- [x1..x2]]

pointsForVent :: Vent -> [Point]
pointsForVent v =
  pointsIfVertical v ++ pointsIfHorizontal v

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

inputPath :: String
inputPath = "./inputs/5.txt"

problem :: Problem
problem = do
  input <- asks envInput
  let (Right vents) = parse parseVentFile "" input
  return $ plotPoints $ concatMap pointsForVent vents
