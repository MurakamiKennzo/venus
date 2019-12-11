-- Given a non-empty array of integers, every element appears three times except for one, which appears exactly once. Find that single one.

module SingleNumber
  (
    singleNumber
  ) where

import Data.List (delete)

singleNumber :: [Int] -> Int
singleNumber [x] = x
singleNumber (x:xs)
  | x `elem` xs = singleNumber (delete x . delete x $ xs)
  | otherwise = x
