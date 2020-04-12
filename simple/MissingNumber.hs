-- Given an array containing n distinct numbers taken from 0, 1, 2, ..., n, find the one that is missing from the array.

module MissingNumber
  (
    missingNumber
  ) where

missingNumber :: [Int] -> Int
missingNumber xs = sum [0 .. length xs] - sum xs
