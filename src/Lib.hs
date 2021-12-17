module Lib
  ( someFunc,
  )
where

import Control.Monad.Trans.Reader (runReader)
import Environment (Env (..))
import Problem8 (problem, inputPath)

someFunc :: IO ()
someFunc = do
  input <- readFile inputPath
  let env = Env {envInput = input}
  putStrLn $ runReader problem env
