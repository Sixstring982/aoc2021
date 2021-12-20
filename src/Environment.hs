module Environment
  ( Env (..),
    EnvReader,
    envLines,
    envIntLines,
    envIntGrid,
    envIntCsv,
  )
where

import Control.Monad.Trans.Reader (Reader)
import qualified Grid as Grid
import Data.List.Split (splitOn)

type EnvReader = Reader Env

data Env = Env {envInput :: String}

envLines :: Env -> [String]
envLines = lines . envInput

envIntLines :: Env -> [Int]
envIntLines = (map read) . envLines

envIntGrid :: Env -> Grid.Grid Int
envIntGrid = fmap (read . (:[])) . Grid.fromLists . envLines

envIntCsv :: Env -> [Int]
envIntCsv = (map read) . (splitOn ",") . envInput
