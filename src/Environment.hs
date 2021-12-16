module Environment
  ( Env (..),
    EnvReader,
    envLines,
    envIntLines,
  )
where

import Control.Monad.Trans.Reader (Reader)

type EnvReader = Reader Env

data Env = Env {envInput :: String}

envLines :: Env -> [String]
envLines = lines . envInput

envIntLines :: Env -> [Int]
envIntLines = (map read) . envLines
