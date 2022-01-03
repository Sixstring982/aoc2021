module Problem9 (problem) where

import Control.Monad.Trans.Reader (asks)
import Data.Array ((!))
import Data.List (intercalate, sort)
import qualified Data.Set as Set
import Environment (envInput)
import qualified Grid as Grid
import Point (Point)
import Point (mooreNeighbors)
import Problem (Problem)
import Text.Parsec

type Parser = Parsec String ()

------------
-- Heightmap
------------

newtype Heightmap = Heightmap (Grid.Grid Int)

instance Show Heightmap where
  show (Heightmap grid) =
    let rows = Grid.gridRows grid
     in intercalate "\n" $ map (concatMap show) rows

--
-- Parsing
--

parseHeightmapRow :: Parser [Int]
parseHeightmapRow = do
  digits <- many digit
  _ <- endOfLine
  return $ map (read . (: [])) digits

parseHeightmap :: Parser Heightmap
parseHeightmap = do
  rows <- many parseHeightmapRow
  _ <- eof
  return $ Heightmap $ Grid.fromLists rows

--
-- Accessors
--

neighbors :: Heightmap -> Point -> [Point]
neighbors (Heightmap grid) p =
  let moores = mooreNeighbors p
   in filter (Grid.isInBounds grid) moores

isLowPoint :: Heightmap -> Point -> Bool
isLowPoint hm@(Heightmap grid) p =
  let ns = neighbors hm p
      neighborValues = map (grid Grid.!?) ns
      currentValue = grid Grid.!? p
   in all (> currentValue) neighborValues

lowPoints :: Heightmap -> [Point]
lowPoints hm@(Heightmap grid) =
  let points = Grid.points grid
   in filter (isLowPoint hm) points

--
-- Finding basins
--

type Basin = Set.Set Point

findBasin :: Heightmap -> Point -> Basin
findBasin hm@(Heightmap grid) lowPoint =
  dfs lowPoint Set.empty [lowPoint]
  where
    dfs _ visited [] = visited
    dfs p visited q =
      let allNeighbors =
            [ n
              | n <- neighbors hm p,
                (grid Grid.!? n) /= Just 9,
                not (n `elem` visited)
            ]
        in dfs (head q) (Set.insert p visited) (allNeighbors ++ (tail q))

findBasins :: Heightmap -> Set.Set Basin
findBasins hm =
  let lows = lowPoints hm
   in Set.fromList $ map (findBasin hm) lows

problem :: Problem
problem = do
  input <- asks envInput
  let (Right hm) = parse parseHeightmap "" input
  return $ show $ product $ take 3 $ reverse $ sort $ map (length . findBasin hm) $ lowPoints hm
