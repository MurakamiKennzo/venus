-- Given an unsorted array of integers, find the length of longest increasing subsequence.

module LengthOfLIS
  (
    lengthOfLIS
  ) where

import Data.List ( findIndex )
import Data.Maybe ( fromMaybe )

lengthOfLIS :: (Ord a) => [a] -> Int
lengthOfLIS [] = 0
lengthOfLIS (x:xs) = lengthOfLIS' x xs `max` lengthOfLIS xs
  where lengthOfLIS' :: (Ord a) => a -> [a] -> Int
        lengthOfLIS' x xs = (1 +) . fromMaybe 0 $ do
          i <- findIndex (> x) xs
          return $ lengthOfLIS' (xs !! i) (drop (succ i) xs)
