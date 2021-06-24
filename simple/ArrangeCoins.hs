-- You have n coins and you want to build a staircase with these coins. The staircase consists of k rows where the ith row has exactly i coins. The last row of the staircase may be incomplete.

-- Given the integer n, return the number of complete rows of the staircase you will build.

module ArrangeCoins
  (
    arrangeCoins
  ) where

arrangeCoins :: Int -> Int
arrangeCoins = arrangeCoins' 1
  where arrangeCoins' :: Int -> Int -> Int
        arrangeCoins' c n
          | n < c = 0
          | otherwise = 1 + arrangeCoins' (succ c) (n - c)
