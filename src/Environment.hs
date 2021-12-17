module Environment
  ( Env (..),
    EnvReader,
    envLines,
    envIntLines,
    envIntCsv,
  )
where

import Control.Monad.Trans.Reader (Reader)
import Data.List.Split (splitOn)

type EnvReader = Reader Env

data Env = Env {envInput :: String}

envLines :: Env -> [String]
envLines = lines . envInput

envIntLines :: Env -> [Int]
envIntLines = (map read) . envLines

envIntCsv :: Env -> [Int]
envIntCsv = (map read) . (splitOn ",") . envInput
