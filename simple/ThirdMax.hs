-- Given integer array nums, return the third maximum number in this array. If the third maximum does not exist, return the maximum number.

module ThirdMax
  (
    thirdMax
  ) where

import Data.Maybe ( fromJust )

thirdMax :: (Ord a, Num a) => [a] -> a
thirdMax xs = fromJust $ thirdMax' (Nothing, Nothing, Nothing) xs

thirdMax' :: (Ord a, Num a) => (Maybe a, Maybe a, Maybe a) -> [a] -> Maybe a
thirdMax' (a, b, c) []
  | c == Nothing = a
  | otherwise = c
thirdMax' (a, b, c) (x:xs)
  | x' > a = thirdMax' (x', a, b) xs
  | x' > b && x' /= a = thirdMax' (a, x', b) xs
  | x' > c && x' /= a && x' /= b = thirdMax' (a, b, x') xs
  | otherwise = thirdMax' (a, b, c) xs
  where x' = return x
