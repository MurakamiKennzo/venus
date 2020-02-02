-- Write a function that takes an unsigned integer and return the number of '1' bits it has (also known as the Hamming weight).

module HammingWeight
  (
    hammingWeight
  ) where

hammingWeight :: Int -> Int
hammingWeight = length . filter (== 1) . toReverseBinary

toReverseBinary :: Int -> [Int]
toReverseBinary 0 = []
toReverseBinary a
  | b == 0 = [c]
  | otherwise = c : toReverseBinary b
  where (b, c) = a `divMod` 2
