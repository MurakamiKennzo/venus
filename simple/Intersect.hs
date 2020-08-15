-- Given two arrays, write a function to compute their intersection.

module Intersect
  (
    intersect
  ) where

intersect :: (Eq a) => [a] -> [a] -> [a]
intersect xs ys = [ x | x <- xs, x `elem` ys ]
