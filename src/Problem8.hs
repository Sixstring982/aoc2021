module Problem8 (problem) where

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

isStrictSuperset :: Ord a => Set.Set a -> Set.Set a -> Bool
superset `isStrictSuperset` set =
  set /= superset
  && all (\e -> e `Set.member` superset) set

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
      nine = fromJust $ find (\ds -> (length ds == 6) && (ds `isStrictSuperset` four)) inputs
      six = fromJust $ find (\ds -> (length ds == 6) && (not (ds `isStrictSuperset` one))) inputs
      zero = fromJust $ find (\ds -> (length ds == 6) && (ds /= six && ds /= nine)) inputs
      five = fromJust $ find (\ds -> (length ds == 5) && (six `isStrictSuperset` ds)) inputs
      three = fromJust $ find (\ds -> (length ds == 5) && (ds `isStrictSuperset` seven)) inputs
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
      matchSegment output =
        case find (\(_, ds) -> ds == output) segmentMap of
          Nothing -> error $ "Can't find " ++ show output ++ "(map = " ++ show segmentMap ++ ")"
          Just (n, _)  -> n
   in map matchSegment outputs

toNumber :: [Int] -> Int
toNumber = go . reverse
  where go [] = 0
        go (d:ds) =
          let prevNum = go ds
           in prevNum * 10 + d

parseExamples :: Parser [Example]
parseExamples = do
  examples <- many parseExample
  _ <- eof
  return examples

problem :: Problem
problem = do
  input <- asks envInput
  let result = parse parseExamples "" input
  let examples = case result of
                    (Left parseError) -> error $ show parseError
                    (Right e) -> e
  let outputs = map (toNumber . determineOutputs) examples
  return $ show $ sum outputs
