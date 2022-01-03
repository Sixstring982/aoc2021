module Problem12 (problem) where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Control.Monad.Trans.Reader (asks)
import Data.Char (isUpper)
import qualified Data.Map.Strict as Map
import Data.Map ((!?))
import qualified Data.MultiMap as MultiMap
import Data.MultiMap ((!), keys, elems)
import qualified Data.Set as Set
import Environment (envInput)
import Graph (Graph)
import Problem (Problem)
import Text.Parsec

type Parser = Parsec String ()

--------
-- Caves
--------

data CaveSize = LargeCave | SmallCave
  deriving (Eq)

newtype Cave = Cave String

caveSize :: Cave -> CaveSize
caveSize (Cave name)
  | isUpper (head name) = LargeCave
  | otherwise = SmallCave

instance Eq Cave where
  (Cave n1) == (Cave n2) = n1 == n2

instance Ord Cave where
  (Cave n1) <= (Cave n2) = n1 <= n2

instance Show Cave where
  show (Cave n) = n

parseCave :: Parser Cave
parseCave = do
  name <- many letter
  return $ Cave name

-------------
-- Cave edges
-------------

type Edge = (Cave, Cave)

parseEdges :: Parser [Edge]
parseEdges = do
  from <- parseCave
  _ <- char '-'
  to <- parseCave
  _ <- endOfLine
  return [(from, to), (to, from)]

--------------
-- Cave system
--------------

type CaveSystem = Graph Cave

parseCaveSystem :: Parser CaveSystem
parseCaveSystem = do
  edges <- many parseEdges
  _ <- eof
  return $ MultiMap.fromList $ concat edges

type Visits = Int

hasVisitedSmallCaveTwice :: CaveSystem -> Map.Map Cave Visits -> Bool
hasVisitedSmallCaveTwice caves visits =
  let visited = [c | c <- keys caves, caveSize c == SmallCave, (fromMaybe 0 (visits !? c)) >= 2]
   in visited /= []

canTravelTo :: CaveSystem -> Map.Map Cave Visits -> Cave -> Bool
canTravelTo caves visits to@(Cave n)
  | caveSize to == LargeCave = True
  | n == "start" = False
  | fromMaybe 0 (visits !? to) >= 1 && hasVisitedSmallCaveTwice caves visits = False
  | fromMaybe 0 (visits !? to) >= 2 = False
  | otherwise = True

findPaths :: CaveSystem -> Cave -> Cave -> [[Cave]]
findPaths caves start to = go (Map.fromList [(start, 0)]) start
  where
    go visits from
      | from == to = [[to]]
      | otherwise =
        let newVisits = Map.insertWith (+) from 1 visits
            nextCaves = [c | c <- caves ! from, canTravelTo caves newVisits c]
            subPaths = concatMap (go newVisits) nextCaves
         in map (from :) subPaths

problem :: Problem
problem = do
  rawEdges <- asks envInput
  let (Right caves) = parse parseCaveSystem "" rawEdges
  return $ show $ length $ findPaths caves (Cave "start") (Cave "end")
