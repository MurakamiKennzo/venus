-- Given an integer array nums, return the maximum result of nums[i] XOR nums[j], where 0 <= i ≤ j < n.

module FindMaximumXOR
  (
    findMaximumXOR
  ) where

import Data.Bits ( xor )

findMaximumXOR :: [Int] -> Int
findMaximumXOR [] = 0
findMaximumXOR [x] = x
findMaximumXOR [x, y] = x `xor` y
findMaximumXOR (x:xs) = max (findMaximumXOR xs) $ maximum [ x `xor` y | y <- xs ]
