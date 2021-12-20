module Graph (Graph) where

import qualified Data.MultiMap as MultiMap

type Graph a = MultiMap.MultiMap a a
