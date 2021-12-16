module Bingo (Draws(..), Space(..), Board(..), parseBingo) where

import Data.List.Split (chunksOf)
import Data.List (intercalate)
import Data.Array (bounds, elems)
import Text.Parsec
import Text.Printf (printf)
import Grid

type Parser = Parsec String ()

--
-- Type definitions
--

type Draws = [Int]

data Space = Space
  { spaceValue :: Int,
    spaceMarked :: Bool
  }
  deriving (Eq)

instance Show Space where
  show (Space v True) = printf "[%2d]" v
  show (Space v False) = printf "(%2d)" v

newtype Board = Board (Grid Space)

instance Show Board where
  show (Board g) = intercalate "\n" $ chunksOf 25 $ intercalate " " $ map show $ elems g

parseDraws :: Parser [Int]
parseDraws = do
  nums <- sepBy (many digit) (char ',')
  _ <- endOfLine
  return $ map read nums

unmarkedSpace :: Int -> Space
unmarkedSpace value = Space value False

--
-- Parsing
--

parseBoardLine :: Parser [Int]
parseBoardLine = do
  _ <- many (char ' ')
  nums <- sepBy (many digit) ((try $ string "  ") <|> (try $ string " "))
  _ <- endOfLine
  return $ map read nums

parseBoard :: Parser Board
parseBoard = do
  _ <- endOfLine
  values <- count 5 parseBoardLine
  return $ Board $ fromLists $ map (map unmarkedSpace) values

parseBingo :: Parser (Draws, [Board])
parseBingo = do
  draws <- parseDraws
  boards <- manyTill parseBoard eof
  return (draws, boards)

--
-- Accessors
--

-- bingoRows :: Board -> [Space]
-- bingoRows (Board g) =
--   let ((minX, minY), (maxX, maxY)) = bounds g
--    in undefined
