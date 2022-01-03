module Problem4 (problem) where

import Bingo (parseBingo, lastBoardToWin, scoreBoard)
import Control.Monad.Trans.Reader (asks)
import Environment (Env (envInput))
import Problem (Problem)
import Text.Parsec (parse)

problem :: Problem
problem = do
  input <- asks envInput
  let Right (draws, boards) = parse parseBingo "" input
  let Just (finalDraw, finalBoard) = lastBoardToWin draws boards
  return $ show $ scoreBoard finalDraw finalBoard
