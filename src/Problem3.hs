module Problem3 (problem) where

import Control.Monad.Trans.Reader (asks)
import Data.Bits ((.&.), bit)
import Environment (envLines)
import Problem (Problem)

fromBits :: String -> Int
fromBits inputBs = go $ reverse inputBs
  where go [] = 0
        go (b:bs) = (if b == '0' then 0 else 1) + 2 * (go bs)

countOnBits :: Int -> [Int] -> Int
countOnBits index nums =
  let mask = bit index
   in length [n | n <- nums, n .&. mask > 0]

mostLeast :: Int -> [Int] -> (Int, Int)
mostLeast index nums =
  let onBits = countOnBits index nums
      numCount = length nums
   in if onBits * 2 >= numCount then (1, 0) else (0, 1)

narrowStep :: Int -> [Int] -> ((Int, Int) -> Int) -> [Int]
narrowStep n ns g =
  let mask = bit n
      ml = mostLeast n ns
   in [num | num <- ns, (if num .&. mask == 0 then 0 else 1) == (g ml)]

narrowBits :: Int -> [Int] -> ((Int, Int) -> Int) -> Int
narrowBits numBits nums g = go (pred numBits) nums
  where go _ (n:[]) = n
        go n ns = go (pred n) (narrowStep n ns g)

problem :: Problem
problem = do
  inputLines <- asks envLines
  let bitCount = length $ head inputLines
  let nums = map fromBits inputLines
  let oxygen = narrowBits bitCount nums fst
  let co2 = narrowBits bitCount nums snd
  return $ show $ oxygen * co2
