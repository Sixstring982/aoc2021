module PointSet
  ( PointSet (..),
    boundsInclusive,
    fromList,
    render,
    toList,
  )
where

import Data.List (intercalate)
import qualified Data.Set as Set
import Point (Point)

newtype PointSet = PointSet (Set.Set Point)

--
-- Constructors
--

fromList :: [Point] -> PointSet
fromList ps = PointSet (Set.fromList ps)

--
-- Accessors
--

member :: Point -> PointSet -> Bool
member p (PointSet ps) = Set.member p ps

toList :: PointSet -> [Point]
toList (PointSet ps) = Set.toList ps

boundsInclusive :: PointSet -> (Point, Point)
boundsInclusive pointSet =
  let ps = toList pointSet
      xs = map fst ps
      ys = map snd ps
   in ((minimum xs, minimum ys), (maximum xs, maximum ys))

render :: (Bool -> String) -> PointSet -> String
render showFn pointSet =
  let ((minX, minY), (maxX, maxY)) = boundsInclusive pointSet
      pss :: [[Bool]] = [[(x, y) `member` pointSet | x <- [minX .. maxX]] | y <- [minY .. maxY]]
      strs :: [[String]] = map (map showFn) pss
   in intercalate "\n" $ map (intercalate "") strs
