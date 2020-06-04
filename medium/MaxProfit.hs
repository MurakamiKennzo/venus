-- Say you have an array for which the ith element is the price of a given stock on day i.

-- Design an algorithm to find the maximum profit. You may complete as many transactions as you like (ie, buy one and sell one share of the stock multiple times) with the following restrictions:

-- You may not engage in multiple transactions at the same time (ie, you must sell the stock before you buy again).
-- After you sell your stock, you cannot buy stock on next day. (ie, cooldown 1 day)

module MaxProfit
  (
    maxProfit
  ) where

import Data.List ( findIndices )

maxProfit :: (Num a, Ord a) => [a] -> a
maxProfit [] = 0
maxProfit (x:xs) = max a c
  where a = maxProfit xs
        b = [ (xs !! i, drop (i + 2) xs) | i <- findIndices (> x) xs ]
        c = if null b then 0 else maximum . map (\(d, e) -> d - x + maxProfit e) $ b
