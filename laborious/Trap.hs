-- Given n non-negative integers representing an elevation map where the width of each bar is 1, compute how much water it is able to trap after raining.

module Trap
  (
    trap
  ) where

import Data.List (group)

trap :: [Int] -> Int
trap a
 | all (==0) a = 0
 | otherwise = trapLevel a + (trap . map (\x -> let b = x - 1 in if b < 0 then 0 else b) $ a)


trapLevel :: [Int] -> Int
trapLevel a = foldl (+) 0 . map length . filter (elem 0) $ c
  where b = group a
        c = tail . init $ b
