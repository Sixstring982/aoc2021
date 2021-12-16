module Problem1 (problem, inputPath) where

import Control.Monad.Trans.Reader (asks)
import Data.List (tails)
import Environment (envIntLines)
import Problem (Problem)

countIncreases :: [Int] -> Int
countIncreases xs@(x : _) = fst $ foldl go (0, x) xs
  where
    go (acc, prev) next =
      if next > prev
        then (succ acc, next)
        else (acc, next)

sumWindows :: Int -> [Int] -> [Int]
sumWindows size = map sum . map (take size) . filter (\x -> length x >= size) . tails

inputPath :: String
inputPath = "./inputs/1.txt"

problem :: Problem
problem = do
  nums <- asks envIntLines
  return $ show $ countIncreases $ sumWindows 3 nums