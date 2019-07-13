-- Given a set of non-overlapping intervals, insert a new interval into the intervals (merge if necessary).

-- You may assume that the intervals were initially sorted according to their start times.

module Insert
  (
    insert
  ) where

import qualified Data.List as L (insert)

insert :: [[Int]] -> [Int] -> [[Int]]
insert xs x = merge . L.insert x $ xs

merge :: [[Int]] -> [[Int]]
merge [] = []
merge [a] = [a]
merge (x:y:z) = let [x1, x2] = x
                    [y1, y2] = y
                in  if y1 > x2 
                      then x : merge (y:z)
                      else merge $ [x1, max x2 y2] : z
