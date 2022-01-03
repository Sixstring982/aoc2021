module Problem14 (problem) where

import Frequency
import Control.Monad.State.Lazy (State, put, get, execState)
import Data.Ord (comparing)
import Data.List (tails, maximumBy, minimumBy, nub)
import qualified Data.Map as Map
import Control.Monad.Trans.Reader (asks)
import Environment (envInput)
import Problem (Problem)
import Text.Parsec hiding (State)

type Parser = Parsec String ()

type Template = String

parseTemplate :: Parser Template
parseTemplate = do
  template <- many upper
  _ <- endOfLine
  return template

type RuleSet = Map.Map (Char, Char) Char

parseRule :: Parser RuleSet
parseRule = do
  (f1:f2:[]) <- count 2 upper
  _ <- string " -> "
  to <- upper
  _ <- endOfLine
  return $ Map.singleton (f1, f2) to

--
-- Formula
--

data Formula = Formula Template RuleSet
  deriving (Show)

parseFormula :: Parser Formula
parseFormula = do
  template <- parseTemplate
  _ <- endOfLine
  rules <- many parseRule
  _ <- eof
  return $ Formula template $ mconcat rules

readFormula :: String -> Either ParseError Formula
readFormula = parse parseFormula ""

--
-- Formula expansion
--

pairs :: [a] -> [(a, a)]
pairs xs = [(t !! 0, t !! 1) | t <- tails xs, length t >= 2]

type Depth = Int

expandPair :: RuleSet -> ((Char, Char), Int) -> [((Char, Char), Int)]
expandPair rules (p@(a, b), pairCount) =
  case rules Map.!? p of
    Just c -> [((a, c), pairCount), ((c, b), pairCount)]
    Nothing -> [(p, pairCount)]

countPairs :: Depth -> Formula -> FrequencyMap (Char, Char)
countPairs targetDepth (Formula template rules) = go targetDepth (frequencies (pairs template))
  where
    go 0 acc = acc
    go depth acc =
      let entries = Map.assocs acc
          newEntries = Map.fromListWith (+) $ concatMap (expandPair rules) entries
       in go (pred depth) newEntries

countOccurrences :: Template -> FrequencyMap (Char, Char) -> Char -> Int
countOccurrences template fs a =
  let fstKeys = [k | k@(ka, _) <- Map.keys fs, ka == a]
      sndKeys = [k | k@(_, ka) <- Map.keys fs, ka == a]
      fsts = sum [fs Map.! k | k <- fstKeys]
      snds = sum [fs Map.! k | k <- sndKeys]
      isFirst = head template == a
      isLast = last template == a
   in (fsts + snds) `div` 2 + if isFirst then 1 else 0 + if isLast then 1 else 0

occurrences :: Depth -> Formula -> Map.Map Char Int
occurrences depth formula@(Formula template rules) =
  let pairCounts = countPairs depth formula
      entries = do
              t <- nub $ template ++ Map.elems rules
              let occs = countOccurrences template pairCounts t
              return (t, occs)
   in Map.fromList entries

--
-- Frequencies
--

scoreFormula :: Depth -> Formula -> Int
scoreFormula depth formula =
  let occs = Map.elems $ occurrences depth formula
   in maximum occs - minimum occs

problem :: Problem
problem = do
  input <- asks envInput
  let (Right formula) = readFormula input
  return $ show $ scoreFormula 40 formula
