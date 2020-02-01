-- Say you have an array for which the i-th element is the price of a given stock on day i.

-- Design an algorithm to find the maximum profit. You may complete at most k transactions.

-- Note:
-- You may not engage in multiple transactions at the same time (ie, you must sell the stock before you buy again).

module MaxProfit2
  (
    maxProfit
  ) where

import Data.List ( findIndices )

maxProfit :: Count -> [Price] -> Profit
maxProfit 0 _ = 0
maxProfit _ [] = 0
maxProfit _ [_] = 0
maxProfit n (x:xs) = let a = maxProfit n xs
                         b = findIndices (> x) xs
                         c = map (\i -> xs !! i - x + maxProfit (pred n) (drop (succ i) xs)) b
                     in  maximum $ a:c

type Count = Int

type Price = Int

type Profit = Int
