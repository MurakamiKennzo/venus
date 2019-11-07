-- Say you have an array for which the ith element is the price of a given stock on day i.

-- Design an algorithm to find the maximum profit. You may complete at most two transactions.

-- Note: You may not engage in multiple transactions at the same time (i.e., you must sell the stock before you buy again).

module MaxProfit
  (
    maxProfit
  ) where

import Data.List (findIndices)

maxProfit :: [Int] -> Int
maxProfit [] = 0
maxProfit [_] = 0
maxProfit (x:xs) = let b = findIndices (> x) xs
                       c = map (subtract x . (xs !!)) b
                       d = map (maxProfit' . flip drop xs . succ) b
                       e = if null b then 0 else maximum $ zipWith (+) c d
                       f = maxProfit xs
                   in  max e f
  where maxProfit' :: [Int] -> Int
        maxProfit' [] = 0
        maxProfit' [_] = 0
        maxProfit' (x:xs) = let a = maximum xs
                            in  if a > x then a - x else 0
