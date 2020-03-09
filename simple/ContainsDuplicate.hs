-- Given an array of integers, find if the array contains any duplicates.

-- Your function should return true if any value appears at least twice in the array, and it should return false if every element is distinct.

module ContainsDuplicate
  (
    containsDuplicate
  ) where

import Data.List ( delete )

containsDuplicate :: (Eq a) => [a] -> Bool
containsDuplicate xs = any (a xs) xs
  where a :: Eq a => [a] -> a -> Bool
        a b = do
          m <- flip delete b
          n <- (`elem` m)
          return n
