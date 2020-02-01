-- Given an array, rotate the array to the right by k steps, where k is non-negative.

module Rotate
  (
    rotate
  ) where

rotate :: [a] -> Int -> [a]
rotate a n = let (b, c) = splitAt (length a - n) a
             in  c <> b
