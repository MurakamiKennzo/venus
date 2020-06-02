-- Given an integer array nums, find the sum of the elements between indices i and j (i ≤ j), inclusive.

-- The update(i, val) function modifies nums by updating the element at index i to val.

module Array
  (
    sumRange
  , update
  ) where

import Control.Monad.State ( State
                           , state )

sumRange :: (Num a) => (Int, Int) -> State [a] a
sumRange a = state $ \s -> (sumRange' a s, s)
  where sumRange' :: (Num a) => (Int, Int) -> [a] -> a
        sumRange' a b = sum . slice a $ b

        slice :: (Int, Int) -> [a] -> [a]
        slice (a, b) xs = take (b - (max a 0) + 1) . drop a $ xs

update :: Int -> a -> State [a] ()
update n a = state $ \s -> ((), update' n a s)
  where update' :: Int -> a -> [a] -> [a]
        update' n a xs = take n xs <> [a] <> drop (1 + (max 0 n)) xs
