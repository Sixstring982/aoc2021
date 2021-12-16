module Problem2 (problem, inputPath) where

import Control.Monad.Trans.Reader (asks)
import Environment (envLines)
import Problem (Problem)

data Sub = Sub
  { subAim :: Int,
    subX :: Int,
    subY :: Int
  }
  deriving (Show)

runCommand :: Sub -> String -> Sub
runCommand (Sub a x y) ('f' : 'o' : 'r' : 'w' : 'a' : 'r' : 'd' : ' ' : n) = Sub a (x + read n) (y + a * read n)
runCommand (Sub a x y) ('d' : 'o' : 'w' : 'n' : n) = Sub (a + read n) x y
runCommand (Sub a x y) ('u' : 'p' : n) = Sub (a - read n) x y

runSub :: [String] -> Sub
runSub = foldl runCommand (Sub 0 0 0)

inputPath :: String
inputPath = "./inputs/2.txt"

problem :: Problem
problem = do
  lines <- asks envLines
  let (Sub a x y) = runSub lines
  return $ show $ x * y
