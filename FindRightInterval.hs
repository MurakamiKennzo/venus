-- You are given an array of intervals, where intervals[i] = [starti, endi] and each starti is unique.

-- The right interval for an interval i is an interval j such that startj >= endi and startj is minimized.

-- Return an array of right interval indices for each interval i. If no right interval exists for interval i, then put -1 at index i.

module FindRightInterval
  (
    findRightInterval
  ) where

import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import Data.List ( sortOn )
import Data.Foldable ( toList )

findRightInterval :: Intervals -> [Int]
findRightInterval xs = let map = buildMap xs
                           a = sortOn fst xs
                           b = sortOn snd xs
                           c = findRightInterval' b a
                       in  toList $ findRightInterval'' (Seq.replicate (length xs) (-1)) c map

buildMap :: Intervals -> IntervalMap
buildMap = buildMap' 0
  where buildMap' :: Int -> Intervals -> IntervalMap
        buildMap' _ [] = mempty
        buildMap' n (x:xs) = Map.insert x n $ buildMap' (succ n) xs 

findRightInterval' :: Intervals -> Intervals -> [(Interval, Maybe Interval)]
findRightInterval' [] [] = []
findRightInterval' (x:xs) [] = (x, Nothing):findRightInterval' xs []
findRightInterval' a@((x, x'):xs) ((y, y'):ys)
  | y >= x' = ((x, x'), return (y, y')):findRightInterval' xs ys
  | otherwise = findRightInterval' a ys

findRightInterval'' :: Seq.Seq Int -> [(Interval, Maybe Interval)] -> IntervalMap -> Seq.Seq Int
findRightInterval'' seq [] _ = seq
findRightInterval'' seq ((a, b):xs) map = 
  case b of
    Nothing -> findRightInterval'' seq xs map
    Just b' -> findRightInterval'' (Seq.update (map Map.! a) (map Map.! b') seq) xs map

type Interval = (Int, Int)
type Intervals = [Interval]
type IntervalMap = Map.Map Interval Int
