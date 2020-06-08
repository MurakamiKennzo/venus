-- Given n balloons, indexed from 0 to n-1. Each balloon is painted with a number on it represented by array nums. You are asked to burst all the balloons. If the you burst balloon i you will get nums[left] * nums[i] * nums[right] coins. Here left and right are adjacent indices of i. After the burst, the left and right then becomes adjacent.

-- Find the maximum coins you can collect by bursting the balloons wisely.

module MaxCoins
  (
    maxCoins
  ) where

maxCoins :: (Num a, Ord a) => [a] -> a
maxCoins [] = 0
maxCoins xs = maximum $ do
  i <- [0 .. length xs - 1]
  let a = calculate i xs
      b = take i xs <> drop (succ i) xs
  return $ a + maxCoins b
  where calculate :: (Num a) => Int -> [a] -> a
        calculate i xs = xs !*! pred i * xs !*! i * xs !*! succ i

        infix 8 !*!
        (!*!) :: (Num a) => [a] -> Int -> a
        xs !*! i
          | i < 0 || i >= length xs = 1
          | otherwise = xs !! i
