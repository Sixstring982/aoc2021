module Problem8 (problem, inputPath) where

import Control.Monad.Trans.Reader (asks)
import Data.List (find)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromJust)
import Environment (envInput)
import Problem (Problem)
import Text.Parsec

type Parser = Parsec String ()

--
-- Wires
--

type Wire = Char

type DigitWires = Set.Set Wire

parseDigitWires :: Parser DigitWires
parseDigitWires = do
  chars <- many $ oneOf "abcdefg"
  return $ Set.fromList chars

is1 :: DigitWires -> Bool
is1 = (== 2) . length

is4 :: DigitWires -> Bool
is4 = (== 4) . length

is7 :: DigitWires -> Bool
is7 = (== 3) . length

is8 :: DigitWires -> Bool
is8 = (== 7) . length

isSuperset :: Ord a => Set.Set a -> Set.Set a -> Bool
superset `isSuperset` set = all (\e -> Set.member e superset) set

--
-- Examples
--

data Example =
  Example
    { exampleInputs :: [DigitWires],
      exampleOutputs :: [DigitWires]
    }
  deriving (Show)

parseExample :: Parser Example
parseExample = do
  inputs <- parseDigitWires `sepBy` (char ' ')
  _ <- string "| "
  outputs <- parseDigitWires `sepBy` (char ' ')
  _ <- endOfLine
  return $ Example inputs outputs

--
-- Segment Maps
--

segmentMapFromInputs :: [DigitWires] -> [(Int, DigitWires)]
segmentMapFromInputs inputs =
  let one  = fromJust $ find is1 inputs
      four = fromJust $ find is4 inputs
      seven = fromJust $ find is7 inputs
      eight = fromJust $ find is8 inputs
      nine = fromJust $ find (\ds -> (length ds == 6) && (ds `isSuperset` four)) inputs
      six = fromJust $ find (\ds -> (length ds == 6) && (not (ds `isSuperset` one))) inputs
      zero = fromJust $ find (\ds -> (length ds == 6) && (ds /= six && ds /= nine)) inputs
      five = fromJust $ find (\ds -> (length ds == 5) && (six `isSuperset` ds)) inputs
      three = fromJust $ find (\ds -> (length ds == 5) && (nine `isSuperset` ds)) inputs
      two = fromJust $ find (\ds -> (length ds == 5) && (ds /= three && ds /= five)) inputs
    in [
        (0, zero),
        (1, one),
        (2, two),
        (3, three),
        (4, four),
        (5, five),
        (6, six),
        (7, seven),
        (8, eight),
        (9, nine)
      ]

determineOutputs :: Example -> [Int]
determineOutputs (Example inputs outputs) =
  let segmentMap = segmentMapFromInputs inputs
   in map (\o -> fst (fromJust (find (\(_, ds) -> ds == o) segmentMap))) outputs

parseExamples :: Parser [Example]
parseExamples = do
  examples <- many parseExample
  _ <- eof
  return examples

inputPath :: String
inputPath = "./inputs/8-demo.txt"

problem :: Problem
problem = do
  input <- asks envInput
  let (Right examples) = parse parseExamples "" input
  return $ show $ map determineOutputs examples
