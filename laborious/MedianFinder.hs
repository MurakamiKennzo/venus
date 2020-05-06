-- Median is the middle value in an ordered integer list. If the size of the list is even, there is no middle value. So the median is the mean of the two middle value.

-- For example,
-- [2,3,4], the median is 3

-- [2,3], the median is (2 + 3) / 2 = 2.5

-- Design a data structure that supports the following two operations:

-- void addNum(int num) - Add a integer number from the data stream to the data structure.
-- double findMedian() - Return the median of all elements so far.

module MedianFinder
  (
    addNum
  , findMedian
  ) where

import Data.List ( insert )
import Control.Monad.State ( state
                           , State )

addNum :: (Num a, Ord a) => a -> State [a] ()
addNum n = state $ \s -> ((), insert n s)

findMedian :: (Num a, Fractional a) => State [a] a
findMedian = state $ \s -> (findMedian' s, s)
  where findMedian' :: (Num a, Fractional a) => [a] -> a
        findMedian' xs
          | b == 0 = (xs !! a + xs !! (pred a)) / 2
          | otherwise = xs !! a
          where (a, b) = length xs `divMod` 2
