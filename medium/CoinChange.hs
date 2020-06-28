-- You are given coins of different denominations and a total amount of money amount. Write a function to compute the fewest number of coins that you need to make up that amount. If that amount of money cannot be made up by any combination of the coins, return -1.

module CoinChange
  (
    coinChange
  ) where

import Data.Maybe ( fromMaybe )

coinChange :: Coins -> Amount -> Int
coinChange xs n = fromMaybe (-1) $ coinChange' xs n

coinChange' :: Coins -> Amount -> Maybe Int
coinChange' _ 0 = return 0
coinChange' [] _ = Nothing
coinChange' xs n
  | n < 0 = Nothing
  | otherwise = let ys = map (fmap (1 +)) . filter (/= Nothing) . map (coinChange' xs) $ [ (n - x) | x <- xs ]
                in  if null ys then Nothing else minimum ys

type Coins = [Int]

type Amount = Int
