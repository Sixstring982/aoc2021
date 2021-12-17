module Problem9 (problem, inputPath) where

import Data.Array ((!))
import Point (mooreNeighbors)
import Control.Monad.Trans.Reader (asks)
import Data.List (intercalate)
import qualified Grid as Grid
import Environment (envInput)
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
  return $ map (read . (:[])) digits

parseHeightmap :: Parser Heightmap
parseHeightmap = do
  rows <- many parseHeightmapRow
  _ <- eof
  return $ Heightmap $ Grid.fromLists rows

--
-- Accessors
--

neighbors :: Heightmap -> (Int, Int) -> [(Int, Int)]
neighbors hm@(Heightmap grid) p =
  let moores = mooreNeighbors p
   in filter (Grid.isInBounds grid) moores

isLowPoint :: Heightmap -> (Int, Int) -> Bool
isLowPoint hm@(Heightmap grid) p =
  let ns = neighbors hm p
      neighborValues = map (grid !) ns
      currentValue = grid ! p
   in all (> currentValue) neighborValues

lowPoints :: Heightmap -> [(Int, Int)]
lowPoints hm@(Heightmap grid) =
  let points = Grid.points grid
   in filter (isLowPoint hm) points

inputPath :: String
inputPath = "./inputs/9.txt"

problem :: Problem
problem = do
  input <- asks envInput
  let (Right hm@(Heightmap grid)) = parse parseHeightmap "" input
  return $ show $ sum $ map (succ . (grid !)) $ lowPoints hm
