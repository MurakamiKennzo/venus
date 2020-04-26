-- Given an array nums containing n + 1 integers where each integer is between 1 and n (inclusive), prove that at least one duplicate number must exist. Assume that there is only one duplicate number, find the duplicate one.

module FindDuplicate
  (
    findDuplicate
  ) where

findDuplicate :: [Int] -> Int
findDuplicate (x:xs)
  | x `elem` xs = x
  | otherwise = findDuplicate xs
