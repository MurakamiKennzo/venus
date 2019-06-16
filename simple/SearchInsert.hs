-- Given a sorted array and a target value, return the index if the target is found. If not, return the index where it would be if it were inserted in order.

-- You may assume no duplicates in the array.

module SearchInsert
  (
    searchInsert
  ) where

type Index = Int

searchInsert :: [Int] -> Int -> Index
searchInsert a b = b `searchInsert'` zip [0 ..] a
  where searchInsert' :: Int -> [(Index, Int)] -> Index
        searchInsert' a [] = 0
        searchInsert' a ((index, v):xs)
          | a <= v = index
          | otherwise = if xs == [] then index + 1 else searchInsert' a xs
