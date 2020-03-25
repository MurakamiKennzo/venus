-- Given an integer array of size n, find all elements that appear more than ⌊ n/3 ⌋ times.

module MajorityElement
  (
    majorityElement
  ) where

import Data.List ( group
                 , sort
                 , genericLength )

majorityElement :: (Num a, Ord a) => [a] -> [a]
majorityElement a = map head . filter ((> genericLength a / 3) . genericLength) . group . sort $ a
