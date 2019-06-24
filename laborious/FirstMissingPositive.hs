-- Given an unsorted integer array, find the smallest missing positive integer.

module FirstMissingPositive
  (
    firstMissingPositive
  ) where

import Data.List (sort)

firstMissingPositive :: [Int] -> Int
firstMissingPositive = position [1 .. ] . sort . filter (> 0)
  where position :: [Int] -> [Int] -> Int
        position (x:xs) [] = x
        position (x:xs) (y:ys)
          | x == y = position xs ys
          | otherwise = x
