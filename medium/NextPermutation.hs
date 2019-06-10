-- Implement next permutation, which rearranges numbers into the lexicographically next greater permutation of numbers.

-- If such arrangement is not possible, it must rearrange it as the lowest possible order (ie, sorted in ascending order).

module NextPermutation
  (
    nextPermutation
  ) where

import Data.List ( sort
                 , nub
                 , permutations )

nextPermutation :: [Int] -> [Int]
nextPermutation xs = 
  let a = dropWhile (/= xs). nub . sort . permutations $ xs
  in  if length a == 1 then reverse xs else a!!1
