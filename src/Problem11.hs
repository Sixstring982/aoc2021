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

flash :: [Point] -> Octopi -> Octopi
flash flashers (Octopi grid) = Octopi $ mapWithIndex mapper grid
  where mapper p value =
          let moores = gridMoores grid p
              flashes = length [m | m <- moores, m `elem` flashers]
           in value + flashes

doFlashes :: Octopi -> Octopi
doFlashes octopi@(Octopi grid) =
  let flashers = findPointsWhere (> 9) grid
   in if length flashers == 0 then octopi
      else doFlashes $ flash flashers octopi

advanceOctopi :: Octopi -> Octopi
advanceOctopi = doFlashes . increaseEnergy

inputPath :: String
inputPath = "./inputs/11-demo.txt"

problem :: Problem
problem = do
  input <- asks envIntGrid
  let octopi = Octopi input
  return $ show $ (!! 2) $ iterate advanceOctopi octopi
