-- Given an array of numbers nums, in which exactly two elements appear only once and all the other elements appear exactly twice. Find the two elements that appear only once.

module SingleNumber2
  (
    singleNumber
  ) where

singleNumber :: (Eq a) => [a] -> [a]
singleNumber [] = []
singleNumber (x:xs)
  | x `notElem` xs = x:singleNumber xs
  | otherwise = singleNumber $ filter (/= x) xs
