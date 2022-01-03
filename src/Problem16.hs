module Problem16 (problem) where

import Control.Monad.Trans.Reader (asks)
import BitsPacket (BitsPacket(..), evaluatePacket, parsePacket)
import Environment (envInput)
import Hex (HexString(..))
import Problem (Problem)

versionSum :: BitsPacket -> Int
versionSum (Literal v _) = v
versionSum (Operator v _ ps) = v + sum (versionSum <$> ps)

problem :: Problem
problem = do
  input <- asks envInput
  let packet = parsePacket (HexString input)
  return $ show $ evaluatePacket packet
