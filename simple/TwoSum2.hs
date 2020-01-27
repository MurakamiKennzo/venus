-- Given an array of integers that is already sorted in ascending order, find two numbers such that they add up to a specific target number.

-- The function twoSum should return indices of the two numbers such that they add up to the target, where index1 must be less than index2.

-- Note:

-- Your returned answers (both index1 and index2) are not zero-based.
-- You may assume that each input would have exactly one solution and you may not use the same element twice.

module TwoSum2
  (
    twoSum
  ) where

import Control.Monad ( guard )
import Data.List ( delete )

twoSum :: (Num a, Eq a) => [a] -> a -> (Int, Int)
twoSum a b = head $ do
  let c = [0 .. length a - 1]
  x <- c
  y <- delete x c
  guard (a !! x + a !! y == b)
  return (x + 1, y + 1)
