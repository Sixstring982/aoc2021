module Emitter
  ( Emitter,
    emit,
    emitted,
    evalEmitter,
    rest,
    getCounter,
  )
where

import Control.Monad.Trans.State

data EmitterState a = EmitterState
  { emitterCounter :: Int,
    emitterAs :: [a],
    emittedAs :: [a]
  }

type Emitter a = State (EmitterState a)

emit :: Show a => Int -> Emitter a [a]
emit n = do
  as <- gets emitterAs
  emitteds <- gets emittedAs
  counter <- gets emitterCounter
  let taken = take n as
  let numTaken = length taken
  () <-
    modify
      ( \s ->
          s
            { emitterCounter = counter + numTaken,
              emitterAs = drop n as,
              emittedAs = emitteds ++ (reverse taken)
            }
      )
  if numTaken < n
    then
      error $
        "emit: Not enough chars [emit = " ++ (show n)
          ++ ", counter = "
          ++ (show counter)
          ++ ", emitted = "
          ++ (show emitteds)
          ++ ", rest = "
          ++ (show as)
          ++ "]"
    else return taken

getCounter :: Emitter a Int
getCounter = gets emitterCounter

emitted :: Emitter a [a]
emitted = gets emittedAs

rest :: Emitter a [a]
rest = gets emitterAs

evalEmitter :: Emitter a b -> [a] -> b
evalEmitter emitter as = evalState emitter (EmitterState 0 as [])
