module Problem12 (problem, inputPath) where

import Data.Char (isUpper)
import qualified Data.MultiMap as MultiMap
import Environment (envInput)
import Control.Monad.Trans.Reader (asks)
import Problem (Problem)
import Text.Parsec
import Graph (Graph)

type Parser = Parsec String ()

--------
-- Caves
--------

data CaveSize = LargeCave | SmallCave

data Cave =
  Cave
    { caveName :: String,
      caveSize :: CaveSize
    }

instance Eq Cave where
  (Cave n1 _) == (Cave n2 _) = n1 == n2

instance Ord Cave where
  (Cave n1 _) <= (Cave n2 _) = n1 <= n2

instance Show Cave where
  show (Cave n _) = n

parseCave :: Parser Cave
parseCave = do
  name <- many letter
  let size = case isUpper (head name) of
               True -> LargeCave
               False -> SmallCave
  return $ Cave name size

-------------
-- Cave edges
-------------

type Edge = (Cave, Cave)

parseEdge :: Parser Edge
parseEdge = do
  from <- parseCave
  _    <- char '-'
  to   <- parseCave
  _    <- endOfLine
  return (from, to)

--------------
-- Cave system
--------------

type CaveSystem = Graph Cave

parseCaveSystem :: Parser CaveSystem
parseCaveSystem = do
  edges <- many parseEdge
  _     <- eof
  return $ MultiMap.fromList edges

inputPath :: String
inputPath = "./inputs/12-demo.txt"

problem :: Problem
problem = do
  rawEdges <- asks envInput
  let (Right caves) = parse parseCaveSystem "" rawEdges
  return $ show $ MultiMap.toMap caves
