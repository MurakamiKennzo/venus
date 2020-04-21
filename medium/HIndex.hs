-- Given an array of citations (each citation is a non-negative integer) of a researcher, write a function to compute the researcher's h-index.

-- According to the definition of h-index on Wikipedia: "A scientist has index h if h of his/her N papers have at least h citations each, and the other N − h papers have no more than h citations each."

module HIndex
  (
    hIndex
  ) where

import Data.List ( sort )    

hIndex :: [Int] -> Int
hIndex = hIndex' . zip [1 ..] . reverse . sort

hIndex' :: [(Int, Int)] -> Int
hIndex' = foldl step 0
  where step :: Int -> (Int, Int) -> Int
        step a (x, y)
          | y >= x = x
          | otherwise = a
