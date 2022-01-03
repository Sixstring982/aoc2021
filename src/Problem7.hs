module Problem7 (problem) where

import Control.Monad.Trans.Reader (asks)
import Data.List (minimumBy)
import Data.Ord (comparing)
import Environment (envIntCsv)
import Problem (Problem)

type Crab = Int

type Position = Int

type Fuel = Int

triangle :: Int -> Int
triangle n = (n * (succ n)) `div` 2

fuelForCrab :: Position -> Crab -> Fuel
fuelForCrab p c = triangle $ abs (p - c)

fuelForCrabs :: Position -> [Crab] -> Fuel
fuelForCrabs p = sum . map (fuelForCrab p)

minFuelForCrabs :: [Crab] -> (Position, Fuel)
minFuelForCrabs crabs =
  let minCrab = minimum crabs
      maxCrab = maximum crabs
      positions = [minCrab..maxCrab]
      minPosition = minimumBy (comparing (\p -> fuelForCrabs p crabs)) positions
   in (minPosition, fuelForCrabs minPosition crabs)

problem :: Problem
problem = do
  crabs <- asks envIntCsv
  let minFuel = minFuelForCrabs crabs
  return $ show $ snd minFuel
