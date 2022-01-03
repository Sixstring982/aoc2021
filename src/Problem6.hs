module Problem6 (problem) where

import qualified Data.Map as Map
import Data.Map ((!?))
import Data.Maybe (fromMaybe)
import Control.Monad.Trans.Reader (asks)
import Environment (envIntCsv)
import Problem (Problem)

type Lanternfish = Int

countFish :: [Lanternfish] -> Map.Map Lanternfish Int
countFish [] = Map.empty
countFish (f:fs) =
  let otherMap = countFish fs
   in Map.insertWith (+) f 1 otherMap

advanceDayForFish :: Lanternfish -> Lanternfish
advanceDayForFish 0 = 6
advanceDayForFish n = pred n

advanceDayForAllFish :: Map.Map Lanternfish Int -> Map.Map Lanternfish Int
advanceDayForAllFish fish =
  let newFishCount = fromMaybe 0 $ fish !? 0
      newFishMap = Map.fromList [(8, newFishCount)]
      oldFishMap = Map.mapKeysWith (+) advanceDayForFish fish
   in Map.unionWith (+) oldFishMap newFishMap

problem :: Problem
problem = do
  fish <- asks envIntCsv
  return $ show $ sum $ Map.elems $ (!! 256) $ iterate advanceDayForAllFish $ countFish fish
