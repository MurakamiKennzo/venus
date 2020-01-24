-- Given an unsorted array, find the maximum difference between the successive elements in its sorted form.

-- Return 0 if the array contains less than 2 elements.

module MaximumGap
  (
    maximumGap
  ) where

import Data.List ( sort )

maximumGap :: (Ord a, Num a) => [a] -> a
maximumGap a = case map (\(x, y) -> y - x) . tuple . sort $ a of
                [] -> 0
                b -> maximum b

tuple :: [a] -> [(a, a)]
tuple [] = []
tuple [_] = []
tuple (a:b:xs) = (a, b) : tuple (b:xs)
