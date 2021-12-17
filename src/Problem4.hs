module Problem4 (problem, inputPath) where

import Bingo (parseBingo, lastBoardToWin, scoreBoard)
import Control.Monad.Trans.Reader (asks)
import Environment (Env (envInput))
import Problem (Problem)
import Text.Parsec (parse)

inputPath :: String
inputPath = "./inputs/4.txt"

problem :: Problem
problem = do
  input <- asks envInput
  let Right (draws, boards) = parse parseBingo "" input
  let Just (finalDraw, finalBoard) = lastBoardToWin draws boards
  -- let Just (n, winner) = winningBoard boards draws
  -- let (Just (_, score)) = runAndScore first draws
  return $ show $ scoreBoard finalDraw finalBoard
