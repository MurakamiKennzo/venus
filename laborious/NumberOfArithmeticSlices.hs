-- Given an integer array nums, return the number of all the arithmetic subsequences of nums.

-- A sequence of numbers is called arithmetic if it consists of at least three elements and if the difference between any two consecutive elements is the same.

-- For example, [1, 3, 5, 7, 9], [7, 7, 7, 7], and [3, -1, -5, -9] are arithmetic sequences.
-- For example, [1, 1, 2, 5, 7] is not an arithmetic sequence.
-- A subsequence of an array is a sequence that can be formed by removing some elements (possibly none) of the array.

-- For example, [2,5,10] is a subsequence of [1,2,1,2,4,1,5,10].
-- The answer is guaranteed to fit in 32-bit integer.

module NumberOfArithmeticSlices
  (
    numberOfArithmeticSlices
  ) where

import qualified Data.Map as Map
import Control.Monad ( forM )
import Control.Monad.State ( state
                           , evalState
                           , State )

numberOfArithmeticSlices :: (Num a, Ord a) => [a] -> Int
numberOfArithmeticSlices a@(x:y:xs) = flip evalState mempty $ do
  xs <- forM [1 .. length a - 1] $ \i -> fmap sum $ forM [0 .. i - 1] $ \j -> setArithmeticSlicesMap (i, j,  a !! i - a !! j)
  return $ sum xs
numberOfArithmeticSlices _ = 0

setArithmeticSlicesMap :: (Num a, Ord a) => (Int, Int, a) -> State (ArithmeticSlicesMap a) Int
setArithmeticSlicesMap (i, j, d) = state $ \s -> go (i, j, d) s
  where go :: (Num a, Ord a) => (Int, Int, a) -> ArithmeticSlicesMap a -> (Int, ArithmeticSlicesMap a)
        go (i, j, d) s = (n, s')
          where k = s Map.!? (j, d)
                n = maybe 0 id k
                s' = case k of 
                      Nothing -> Map.insertWith (+) (i, d) 1 s
                      Just n -> Map.insertWith (+) (i, d) (n + 1) s

type ArithmeticSlicesMap a = Map.Map (Int, a) Int
