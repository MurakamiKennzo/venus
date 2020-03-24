-- Given a sorted integer array without duplicates, return the summary of its ranges.

module SummaryRanges
  (
    summaryRanges
  ) where

import Data.List ( sort )

summaryRanges :: [Int] -> [String]
summaryRanges = map format . groupBy' . sort

groupBy' :: [Int] -> [[Int]]
groupBy' [] = []
groupBy' [x] = [[x]]
groupBy' (x:y:xs) = let b = groupBy' (y:xs)
                    in  if succ x == y
                          then (x:head b):tail b
                          else [x]:b

format :: [Int] -> String
format [] = ""
format [x] = show x
format a = (show $ head a) <> "->" <> (show $ last a)
