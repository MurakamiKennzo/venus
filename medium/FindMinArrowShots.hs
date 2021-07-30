-- There are some spherical balloons spread in two-dimensional space. For each balloon, provided input is the start and end coordinates of the horizontal diameter. Since it's horizontal, y-coordinates don't matter, and hence the x-coordinates of start and end of the diameter suffice. The start is always smaller than the end.

-- An arrow can be shot up exactly vertically from different points along the x-axis. A balloon with xstart and xend bursts by an arrow shot at x if xstart ≤ x ≤ xend. There is no limit to the number of arrows that can be shot. An arrow once shot keeps traveling up infinitely.

-- Given an array points where points[i] = (xstart, xend), return the minimum number of arrows that must be shot to burst all balloons.

module FindMinArrowShots
  (
    findMinArrowShots
  ) where

import Data.List ( sortOn )

findMinArrowShots :: (Num a, Ord a) => [(a, a)] -> Int
findMinArrowShots [] = 0
findMinArrowShots xs = let xs' = sortOn fst xs
                       in  findMinArrowShots' (0, snd $ xs' !! 0) . drop 1 $ xs'
  where findMinArrowShots' :: (Num a, Ord a) => (Int, a) -> [(a, a)] -> Int
        findMinArrowShots' (n, _) [] = n + 1
        findMinArrowShots' (n, p) ((x', x''):xs)
          | x' > p = findMinArrowShots' (succ n, x'') xs
          | otherwise = findMinArrowShots' (n, min p x'') xs
