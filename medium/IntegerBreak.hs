-- Given a positive integer n, break it into the sum of at least two positive integers and maximize the product of those integers. Return the maximum product you can get.

module IntegerBreak
  (
    integerBreak
  ) where

integerBreak :: Int -> Int
integerBreak n
  | n < 2 = 0
integerBreak 2 = 1
integerBreak n = maximum [ max (x * integerBreak y) (x * y) | x <- [1 .. n], let y = n - x]
