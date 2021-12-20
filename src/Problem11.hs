module Problem11 (problem, inputPath) where

import Point (Point)
import Control.Monad.Trans.Reader (asks)
import Data.Array ((!), bounds)
import Data.List (intercalate)
import Environment (envInput, envIntGrid)
import qualified Data.Set
import Grid (Grid, mapWithIndex, mapWithMooreValues, findPointsWhere, gridMoores)
import qualified Grid as Grid (toLists)
import Problem (Problem)

--
-- Octopi
--

type Octopus = Int

newtype Octopi = Octopi (Grid Octopus)

instance Show Octopi where
  show (Octopi g) = intercalate "\n" $ map concat $ map (map show) $ Grid.toLists g

increaseEnergy :: Octopi -> Octopi
increaseEnergy (Octopi grid) = Octopi $ fmap succ grid

flashPoint :: Octopi -> Point -> Octopi
flashPoint (Octopi grid) flasher =
  let neighbors = gridMoores grid flasher
      mapper point value
        | value == 0 = 0
        | point == flasher = 0
        | point `elem` neighbors = succ value
        | otherwise = value
   in Octopi $ mapWithIndex mapper grid

flash :: [Point] -> Octopi -> Octopi
flash flashPoints octopi = foldl flashPoint octopi flashPoints

doFlashes :: Octopi -> Octopi
doFlashes octopi@(Octopi grid) =
  let flashPoints = findPointsWhere (> 9) grid
   in if length flashPoints == 0
      then octopi
      else doFlashes $ flash flashPoints octopi

advanceOctopi :: Octopi -> Octopi
advanceOctopi = doFlashes . increaseEnergy

countFlashes :: Int -> Octopi -> Int
countFlashes = go 0
  where
    go flashCount 0         _ = flashCount
    go flashCount stepsLeft octopi =
          let next@(Octopi grid) = advanceOctopi octopi
              flashes = length $ findPointsWhere (== 0) grid
           in go (flashCount + flashes) (pred stepsLeft) next

allFlashStep :: Octopi -> Int
allFlashStep = go 0
  where
    go step octopi@(Octopi grid)
      | all (== 0) $ concat $ Grid.toLists grid = step
      | otherwise = go (succ step) (advanceOctopi octopi)

inputPath :: String
inputPath = "./inputs/11.txt"

problem :: Problem
problem = do
  input <- asks envIntGrid
  let octopi = Octopi input
  return $ show $ allFlashStep octopi
