-- Given a non-empty array of integers, every element appears twice except for one. Find that single one.

module SingleNumber
  (
    singleNumber
  ) where

import Data.List (delete)

singleNumber :: [Int] -> Int
singleNumber [x] = x
singleNumber (x:xs)
  | x `elem` xs = singleNumber (delete x xs)
  | otherwise = x
