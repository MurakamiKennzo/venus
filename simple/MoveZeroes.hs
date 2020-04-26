-- Given an array nums, write a function to move all 0's to the end of it while maintaining the relative order of the non-zero elements.

module MoveZeroes
  (
    moveZeroes
  ) where

import Data.List ( partition )    

moveZeroes :: (Num a, Eq a) => [a] -> [a]
moveZeroes xs = let (a, b) = partition (0 ==) xs
                in  b <> a
