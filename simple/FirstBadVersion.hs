-- You are a product manager and currently leading a team to develop a new product. Unfortunately, the latest version of your product fails the quality check. Since each version is developed based on the previous version, all the versions after a bad version are also bad.

-- Suppose you have n versions [1, 2, ..., n] and you want to find out the first bad one, which causes all the following ones to be bad.

-- You are given an API bool isBadVersion(version) which will return whether version is bad. Implement a function to find the first bad version.

module FirstBadVersion
  (
    firstBadVersion
  ) where

import Data.List ( sort )

firstBadVersion :: (Ord a) => (a -> Bool) -> [a] -> a
firstBadVersion f = foldr1 (step f) . sort
  where step :: (a -> Bool) -> a -> a -> a
        step f a b 
          | f a = a
          | otherwise = b
