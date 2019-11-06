-- Say you have an array for which the ith element is the price of a given stock on day i.

-- If you were only permitted to complete at most one transaction (i.e., buy one and sell one share of the stock), design an algorithm to find the maximum profit.

-- Note that you cannot sell a stock before you buy one.

module MaxProfit 
  (
    maxProfit
  ) where

maxProfit :: [Int] -> Int
maxProfit [] = 0
maxProfit [a] = 0
maxProfit [a, b] = if a > b then 0 else b - a
maxProfit (x:xs) = let a = maximum xs - x
                   in  max a (maxProfit xs)
