module Problem15 (problem) where

import Control.Monad.Trans.Reader (asks)
import Control.Monad.Trans.State (State, execState, get, gets, modify, put)
import Data.List (sortBy)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (comparing)
import qualified Data.PQueue.Min as PQ
import Data.Tuple (swap)
import Debug.Trace
import Environment (envLines)
import Grid as Grid
import Point (Point, manhattan)
import Problem (Problem)
import Text.Printf

--
-- Helpers
--

manhattanPoints :: Grid a -> [Point]
manhattanPoints = sortBy (comparing manhattan) . points

leftPad :: Int -> String
leftPad = printf "%1d"

--
-- Grid expansion
--

valueOffset :: Int -> Int -> Int
valueOffset o v
  | o + v > 9 = valueOffset (o - 9) v
  | otherwise = o + v

expandGrid :: Grid Int -> Grid Int
expandGrid smallGrid =
  let (sw, sh) = Grid.size smallGrid
      bigSize = (sw * 5, sh * 5)
      pointFn (x, y) =
        let offset = x `div` sw + y `div` sh
            smallPoint = (x `rem` sw, y `rem` sh)
            value = smallGrid Grid.! smallPoint
         in valueOffset value offset
   in Grid.generate bigSize pointFn

--
-- Risk evaluation
--

updateRisk :: Point -> Grid Int -> Grid Int
updateRisk p@(x, y) grid =
  let up = grid Grid.!? (x, y - 1)
      left = grid Grid.!? (x - 1, y)
   in case catMaybes [left, up] of
        [] -> grid
        xs -> Grid.insert p ((minimum xs) + grid Grid.! p) grid

cascadeRisk :: Grid Int -> Grid Int
cascadeRisk initialGrid = go initialGrid (manhattanPoints initialGrid)
  where
    go grid [] = grid
    go grid (p : ps) = go (updateRisk p grid) ps

lowestRiskPathValue :: Grid Int -> Int
lowestRiskPathValue grid =
  let lowerRightPoint = Grid.boundsInclusive grid
      upperLeftValue = grid Grid.! (0, 0)
   in (\x -> x - upperLeftValue) $ (Grid.! lowerRightPoint) $ cascadeRisk grid

--
-- Risk evaluation: Dijkstra
--

data PriorityPoint = PriorityPoint
  { priority :: Int,
    point :: Point
  }
  deriving (Eq)

instance Ord PriorityPoint where
  (PriorityPoint a _) <= (PriorityPoint b _) = a <= b

data DijkstraState = DijkstraState
  { djGrid :: Grid Int,
    djPqueue :: PQ.MinQueue PriorityPoint,
    djDists :: Map.Map Point Int,
    djPrevs :: Map.Map Point Point
  }

initDijkstraState :: Grid Int -> DijkstraState
initDijkstraState grid = DijkstraState grid pq dists prevs
  where
    distsList = [(p, if p == (0, 0) then 0 else 1_000_000_000) | p <- points grid]
    dists = Map.fromList distsList
    prevs = Map.empty
    pq = PQ.fromList $ (\(p, pri) -> PriorityPoint pri p) <$> distsList

popQueue :: State DijkstraState PriorityPoint
popQueue = do
  (p, newQueue) <- PQ.deleteFindMin <$> gets djPqueue
  _ <- modify (\s -> s {djPqueue = newQueue})
  return p

updateNeighbor :: Point -> Point -> State DijkstraState ()
updateNeighbor u v = do
  grid <- gets djGrid
  pq <- gets djPqueue
  dist <- gets djDists
  prev <- gets djPrevs
  let ps = point <$> PQ.toListU pq
  if not (v `elem` ps)
    then return ()
    else do
      let alt = (dist Map.! u) + (grid Grid.! v)
      if alt >= (dist Map.! v)
        then return ()
        else do
          let newDist = Map.insert v alt dist
          let newPrev = Map.insert v u prev
          let newQ = PQ.map (\p -> p {priority = if (point p) == v then alt else (priority p)}) pq
          put $ DijkstraState grid newQ newDist newPrev

lowestRiskPathDijkstra :: Grid Int -> DijkstraState
lowestRiskPathDijkstra = execState go . initDijkstraState
  where
    go = do
      (PriorityPoint _ u@(x, y)) <- popQueue
      _ <- updateNeighbor u (x + 1, y)
      _ <- updateNeighbor u (x, y + 1)
      _ <- updateNeighbor u (x - 1, y)
      _ <- updateNeighbor u (x, y - 1)
      pq <- gets djPqueue
      if PQ.null pq
        then return ()
        else go

path :: Point -> Point -> DijkstraState -> Maybe [Point]
path from to state = go to []
  where
    prevs = djPrevs state
    go current acc
      | current == from = Just $ reverse (current : acc)
      | otherwise = case prevs Map.!? current of
        Nothing -> Nothing
        Just next -> go next (current : acc)

problem :: Problem
problem = do
  input <- asks envLines
  let grid :: Grid Int = expandGrid $ read <$> (: []) <$> Grid.fromLists input
  let lowerRightPoint = Grid.boundsInclusive grid
  return $ show $ (Map.! lowerRightPoint) $ djDists $ lowestRiskPathDijkstra $ grid

-- return $ show $ lowestRiskPathValue $ expandGrid grid
