module Problem4 (problem, inputPath) where

import Bingo (Board (..), Draws (..), Space (..), parseBingo)
import Control.Monad.Trans.Reader (asks)
import Data.List (find, sortBy, transpose)
import Data.Ord (comparing)
import Environment (Env (envInput), envLines)
import Grid (Grid, fromLists)
import Problem (Problem)
import Text.Parsec (parse)

-- winningBoard :: [Board] -> Draws -> Maybe (Int, Board)
-- winningBoard boards [] = Nothing
-- winningBoard boards (d : draws) =
--   let newBoards = map (\b -> applyDraw b d) boards
--    in case find bingo newBoards of
--         Just b -> Just (d, b)
--         otherwise -> winningBoard newBoards draws

-- firstBoard :: [Board] -> Draws -> Maybe Board
-- firstBoard boards draws =
--   let sortFn board =
--         case runAndScore board draws of
--           Nothing -> (0, 0)
--           Just (a, b) -> (a, b)
--       sortedBoards = sortBy (comparing sortFn) boards
--    in if sortedBoards == [] then Nothing else Just $ head sortedBoards

inputPath :: String
inputPath = "./inputs/4.txt"

problem :: Problem
problem = do
  input <- asks envInput
  let Right (draws, boards) = parse parseBingo "" input
  -- let Just (n, winner) = winningBoard boards draws
  -- let (Just (_, score)) = runAndScore first draws
  return $ show $ head boards
