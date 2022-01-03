module Problem13 (problem) where

import Data.Maybe (isJust)
import Control.Monad.Trans.Reader (asks)
import Data.List (intercalate)
import qualified Data.Set as Set
import Environment (envInput)
import qualified Grid as Grid
import Grid ((!?), boundsInclusive)
import Point (Point)
import PointSet (PointSet, fromList, toList)
import qualified PointSet as PointSet
import Problem (Problem)
import Text.Parsec

type Parser = Parsec String ()

-----------------
-- Fold direction
-----------------

data FoldDirection = Horizontal | Vertical
  deriving (Show)

parseFoldDirection :: Parser FoldDirection
parseFoldDirection = do
  direction <- oneOf "xy"
  return $ case direction of
    'x' -> Horizontal
    'y' -> Vertical

--------
-- Folds
--------

data Fold = Fold FoldDirection Int
  deriving (Show)

parseFold :: Parser Fold
parseFold = do
  _ <- string "fold along "
  dir <- parseFoldDirection
  _ <- char '='
  n <- many digit
  _ <- endOfLine
  return $ Fold dir (read n)

------------
-- DotMatrix
------------

toDot :: Bool -> Char
toDot True = '#'
toDot False = '.'

type DotMatrix = PointSet

parsePoint :: Parser Point
parsePoint = do
  x <- many digit
  _ <- char ','
  y <- many digit
  _ <- endOfLine
  return (read x, read y)

parseDotMatrix :: Parser DotMatrix
parseDotMatrix = do
  points <- many parsePoint
  return $ dotMatrixFromPoints points

dotMatrixFromPoints :: [Point] -> DotMatrix
dotMatrixFromPoints = PointSet.fromList

--
-- Origami
--

data Origami = Origami DotMatrix [Fold]

instance Show Origami where
  show (Origami dm folds) = (PointSet.render ((:[]) . toDot) dm) ++ "\n\n" ++ (show folds)

parseOrigami :: Parser Origami
parseOrigami = do
  dm <- parseDotMatrix
  _ <- endOfLine
  folds <- many parseFold
  _ <- eof
  return $ Origami dm folds

--
-- Folding
--

reflect :: Fold -> Point -> Point
reflect (Fold Vertical ry) (x, y)
  | y < ry = (x, y)
  | otherwise = (x, y + 2 * (ry - y))
reflect (Fold Horizontal rx) (x, y)
  | x < rx = (x, y)
  | otherwise =  (x + 2 * (rx - x), y)

applyFold :: Fold -> DotMatrix -> DotMatrix
applyFold fold dm =
  let points = PointSet.toList dm
      reflected = map (reflect fold) points
   in PointSet.fromList reflected

foldOnce :: Origami -> Origami
foldOnce o@(Origami _ []) = o
foldOnce (Origami dm (f : fs)) =
  Origami (applyFold f dm) fs

foldAll :: Origami -> Origami
foldAll o@(Origami _ []) = o
foldAll o = foldAll $ foldOnce $ o

problem :: Problem
problem = do
  input <- asks envInput
  let (Right origami) = parse parseOrigami "" input
  let folded = foldAll origami
  return $ show folded
