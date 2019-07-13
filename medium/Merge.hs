-- Given a collection of intervals, merge all overlapping intervals.

module Merge
  (
    merge
  ) where

import Data.List (sort)

merge :: [[Int]] -> [[Int]]
merge = merge' . sort

merge' :: [[Int]] -> [[Int]]
merge' [] = []
merge' [a] = [a]
merge' (x:y:z) = let [x1, x2] = x
                     [y1, y2] = y
                 in  if y1 > x2 
                        then x : merge' (y:z)
                        else merge' $ [x1, max x2 y2] : z

