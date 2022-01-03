module Problem11 (problem) where

import Control.Monad.Trans.Reader (asks)
import Data.Array ((!), bounds)
import Data.List (intercalate)
import qualified Data.Set
import Environment (envInput, envIntGrid)
import Grid (Grid, gridMoores, mapWithIndex, pointsWhere, render, values)
import qualified Grid as Grid (toLists)
import Point (Point)
import Problem (Problem)

--
-- Octopi
--

type Octopus = Int

newtype Octopi = Octopi (Grid Octopus)

instance Show Octopi where
  show (Octopi g) = Grid.render "?" g

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
  let flashPoints = pointsWhere (any (> 9)) grid
   in if length flashPoints == 0
        then octopi
        else doFlashes $ flash flashPoints octopi

advanceOctopi :: Octopi -> Octopi
advanceOctopi = doFlashes . increaseEnergy

countFlashes :: Int -> Octopi -> Int
countFlashes = go 0
  where
    go flashCount 0 _ = flashCount
    go flashCount stepsLeft octopi =
      let next@(Octopi grid) = advanceOctopi octopi
          flashes = length $ pointsWhere (any (== 0)) grid
       in go (flashCount + flashes) (pred stepsLeft) next

allFlashStep :: Octopi -> Int
allFlashStep = go 0
  where
    go step octopi@(Octopi grid)
      | all (== 0) $ Grid.values grid = step
      | otherwise = go (succ step) (advanceOctopi octopi)

problem :: Problem
problem = do
  input <- asks envIntGrid
  let octopi = Octopi input
  return $ show $ allFlashStep octopi
