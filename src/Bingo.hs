{-# LANGUAGE ScopedTypeVariables#-}

module Bingo (Draws, Space(..), Board(..), parseBingo, runGame, lastBoardToWin, scoreBoard) where

import Data.List.Split (chunksOf)
import Data.List (find, intercalate, partition)
import Data.Array (elems)
import Text.Parsec (
  many, digit, Parsec, char, sepBy, endOfLine, try, string, count, manyTill, eof, (<|>))
import Text.Printf (printf)
import Grid

type Parser = Parsec String ()

--
-- Type definitions
--

type Draws = [Int]

data Space = Marked Int | Unmarked Int
  deriving (Eq)

instance Show Space where
  show (Marked v) = printf "[%2d]" v
  show (Unmarked v) = printf "(%2d)" v

newtype Board = Board (Grid Space)

instance Show Board where
  show (Board g) =
    let rows :: [[Space]] = chunksOf 5 $ elems g
        shownRows :: [[String]] = map (map show) rows
     in intercalate "\n" $ (map (intercalate " ")) shownRows

parseDraws :: Parser [Int]
parseDraws = do
  nums <- sepBy (many digit) (char ',')
  _ <- endOfLine
  return $ map read nums

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
  return $ Board $ fromLists $ map (map Unmarked) values

parseBingo :: Parser (Draws, [Board])
parseBingo = do
  draws <- parseDraws
  boards <- manyTill parseBoard eof
  return (draws, boards)

--
-- Accessors
--

spaceValue :: Space -> Int
spaceValue (Marked x) = x
spaceValue (Unmarked x) = x

isMarked :: Space -> Bool
isMarked (Marked _) = True
isMarked (Unmarked _) = False

boardRow :: Int -> Board -> [Space]
boardRow y (Board g) = gridRow y g

boardCol :: Int -> Board -> [Space]
boardCol x (Board g) = gridCol x g

boardRows :: Board -> [[Space]]
boardRows board = [boardRow y board | y <- [0..4]]

boardCols :: Board -> [[Space]]
boardCols board = [boardCol x board | x <- [0..4]]

isBingo :: Board -> Bool
isBingo board =
  let cols = boardCols board
      rows = boardRows board
      candidates = cols ++ rows
   in any (all isMarked) candidates

scoreBoard :: Int -> Board -> Int
scoreBoard lastDraw (Board g) =
  let unmarked = map spaceValue $ filter (not . isMarked) $ elems g
   in lastDraw * (sum unmarked)

--
-- Mutators
--

mark :: Space -> Space
mark (Unmarked x) = Marked x
mark x = x

drawNumber :: Int -> Board -> Board
drawNumber n (Board g) = Board $ fmap draw g
  where draw space
          | n == spaceValue space = mark space
          | True = space

lastBoardToWin :: [Int] -> [Board] -> Maybe (Int, Board)
lastBoardToWin [] _ = Nothing
lastBoardToWin (d:draws) boards =
  let newBoards = map (drawNumber d) boards
      (winners, losers) = partition isBingo newBoards
   in if length winners == 1 && length boards == 1
      then Just (d, head winners)
      else lastBoardToWin draws losers


runGame :: [Int] -> [Board] -> Maybe (Int, Board)
runGame [] _ = Nothing
runGame (d:draws) boards =
  let newBoards = map (drawNumber d) boards
   in case find isBingo newBoards of
        Just board -> Just (d, board)
        Nothing -> runGame draws newBoards
