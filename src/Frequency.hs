module Frequency (FrequencyMap, frequencies, addFrequencies, subtractFrequencies) where

import Data.Map (Map, fromListWith)
import Data.Map.Merge.Strict (mapMissing, merge, preserveMissing, zipWithMatched)

type FrequencyMap a = Map a Int

frequencies :: Ord a => [a] -> FrequencyMap a
frequencies as = fromListWith (+) $ map (\k -> (k, 1)) as

addFrequencies :: Ord a => FrequencyMap a -> FrequencyMap a -> FrequencyMap a
addFrequencies = merge preserveMissing preserveMissing (zipWithMatched (\_ a b -> a + b))

subtractFrequencies :: Ord a => FrequencyMap a -> FrequencyMap a -> FrequencyMap a
subtractFrequencies = merge preserveMissing (mapMissing (\_ x -> x * (-1))) (zipWithMatched (\_ a b -> a - b))
