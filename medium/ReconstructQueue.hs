-- You are given an array of people, people, which are the attributes of some people in a queue (not necessarily in order). Each people[i] = [hi, ki] represents the ith person of height hi with exactly ki other people in front who have a height greater than or equal to hi.

-- Reconstruct and return the queue that is represented by the input array people. The returned queue should be formatted as an array queue, where queue[j] = [hj, kj] is the attributes of the jth person in the queue (queue[0] is the person at the front of the queue).

module ReconstructQueue
  (
    reconstructQueue
  ) where

import Data.Function ( on )
import Data.List ( sortBy )

reconstructQueue :: (Ord a) => [(a, Int)] -> [(a, Int)]
reconstructQueue = foldl foldlFn [] . sortBy (\(x, y) (x', y') -> flip compare x x' <> compare y y')
  where foldlFn :: [(a, Int)] -> (a, Int) -> [(a, Int)]
        foldlFn xs x@(_, n)
          | length xs > n = take n xs <> [x] <> drop n xs
          | otherwise = xs <> [x]
