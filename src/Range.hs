module Range (range) where

range :: (Enum a, Ord a) => a -> a -> [a]
range from to
  | from == to = [from]
  | from < to = [from..to]
  | from > to = [from,(pred from)..to]
