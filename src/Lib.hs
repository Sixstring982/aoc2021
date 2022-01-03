module Lib
  ( someFunc,
  )
where

import Control.Monad.Trans.Reader (runReader)
import Environment (Env (..))
import Problem16 (problem)
import System.Environment (getArgs)

someFunc :: IO ()
someFunc = do
  args <- getArgs
  input <- readFile (head args)
  let env = Env {envInput = input}
  putStrLn $ runReader problem env
