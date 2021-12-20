module Problem10 (problem, inputPath) where

import Control.Monad.Trans.Reader (asks)
import Data.Maybe (catMaybes)
import Data.List (sort)
import qualified Data.Set as Set
import Environment (envLines)
import Problem (Problem)

--
-- Brackets
--

openBrackets :: Set.Set Char
openBrackets = Set.fromList ['(', '{', '[', '<']

closeBrackets :: Set.Set Char
closeBrackets = Set.fromList [')', '}', ']', '>']

openBracket :: Char -> Bool
openBracket x = x `Set.member` openBrackets

closeBracket :: Char -> Bool
closeBracket x = x `Set.member` closeBrackets

opposingBracket :: Char -> Char
opposingBracket '(' = ')'
opposingBracket ')' = '('
opposingBracket '[' = ']'
opposingBracket ']' = '['
opposingBracket '{' = '}'
opposingBracket '}' = '{'
opposingBracket '<' = '>'
opposingBracket '>' = '<'
opposingBracket _ = undefined

--
-- Syntax classifier
--

data Corruption = Corruption {corruptionExpected :: Char, corruptionGot :: Char}
  deriving (Show)

scoreCorruption :: Corruption -> Int
scoreCorruption (Corruption _ ')') = 3
scoreCorruption (Corruption _ ']') = 57
scoreCorruption (Corruption _ '}') = 1197
scoreCorruption (Corruption _ '>') = 25137
scoreCorruption _ = undefined

newtype Completion = Completion [Char]
  deriving (Show)

scoreCompletion :: Completion -> Int
scoreCompletion (Completion chars) = go chars 0
  where
    go [] score = score
    go (')' : cs) score = go cs (1 + 5 * score)
    go (']' : cs) score = go cs (2 + 5 * score)
    go ('}' : cs) score = go cs (3 + 5 * score)
    go ('>' : cs) score = go cs (4 + 5 * score)
    go _ _ = undefined

data Classification
  = Ok
  | Incomplete Completion
  | Corrupted [Corruption]
  | BadCharacter Char
  deriving (Show)

scoreCorruptionClassification :: Classification -> Maybe Int
scoreCorruptionClassification (Corrupted cs) = Just $ sum $ map scoreCorruption cs
scoreCorruptionClassification _ = Nothing

scoreIncompleteClassification :: Classification -> Maybe Int
scoreIncompleteClassification (Incomplete c) = Just $ scoreCompletion c
scoreIncompleteClassification _ = Nothing

classifyLine :: String -> Classification
classifyLine line = go (tail line) [head line] []
  where
    go [] [] [] = Ok
    go [] stack [] = Incomplete $ Completion (map opposingBracket stack)
    go [] _ cs = Corrupted cs
    go (x : xs) stack cs
      | openBracket x = go xs (x : stack) cs
      | closeBracket x =
        let expected = opposingBracket (head stack)
         in if x == expected
              then go xs (tail stack) cs
              else go xs (tail stack) ((Corruption expected x) : cs)
      | otherwise = BadCharacter x

middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

inputPath :: String
inputPath = "./inputs/10.txt"

problem :: Problem
problem = do
  inputLines <- asks envLines
  return $ show $ middle $ sort $ catMaybes $ map (scoreIncompleteClassification . classifyLine) inputLines
