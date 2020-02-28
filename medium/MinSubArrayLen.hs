-- Given an array of n positive integers and a positive integer s, find the minimal length of a contiguous subarray of which the sum ≥ s. If there isn't one, return 0 instead.

module MinSubArrayLen
  (
    minSubArrayLen
  ) where

import Data.List ( sort )
import Data.Maybe ( fromMaybe
                  , listToMaybe
                  , catMaybes )

minSubArrayLen :: Int -> [Int] -> Int
minSubArrayLen n xs = fromMaybe 0 . listToMaybe . sort . catMaybes $ subArrayLen n xs

subArrayLen :: Int -> [Int] -> [Maybe Int]
subArrayLen _ [] = []
subArrayLen n a@(_:xs) = startArrayLen n a : subArrayLen n xs

startArrayLen :: Int -> [Int] -> Maybe Int
startArrayLen n xs
  | n <= 0 = Just 0
  | null xs = Nothing
  | otherwise = fmap (1 +) $ startArrayLen (n - head xs) (tail xs)
