-- Say you have an array for which the ith element is the price of a given stock on day i.

-- Design an algorithm to find the maximum profit. You may complete as many transactions as you like (i.e., buy one and sell one share of the stock multiple times).

-- Note: You may not engage in multiple transactions at the same time (i.e., you must sell the stock before you buy again).

module MaxProfit2
  (
    maxProfit
  ) where

import Data.List (findIndices)

maxProfit :: [Int] -> Int
maxProfit [] = 0
maxProfit [a] = 0
maxProfit (x:xs) = let b = findIndices (> x) xs
                       c = map ((subtract x) . (xs !!)) b
                       d = map (flip drop xs . succ) b
                       e = map maxProfit d
                       f = if null b then 0 else maximum $ zipWith (+) c e
                       g = maxProfit xs
                   in  max f g
