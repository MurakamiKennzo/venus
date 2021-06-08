-- Given an array of intervals intervals where intervals[i] = [starti, endi], return the minimum number of intervals you need to remove to make the rest of the intervals non-overlapping.

module EraseOverlapIntervals
  (
    eraseOverlapIntervals
  ) where

import Data.List ( sort )

eraseOverlapIntervals :: (Ord a) => Intervals a -> Int
eraseOverlapIntervals = eraseOverlapIntervals' . sort

eraseOverlapIntervals' :: (Ord a) => Intervals a -> Int
eraseOverlapIntervals' [] = 0
eraseOverlapIntervals' [_] = 0
eraseOverlapIntervals' ((x, x'):(y, y'):xs)
  | y >= x' = eraseOverlapIntervals' $ (y, y'):xs
  | otherwise = (1+) . eraseOverlapIntervals' $ (if y' > x' then (x, x') else (y, y')):xs

type Intervals a = [(a, a)]
