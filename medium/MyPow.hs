-- Implement pow(x, n), which calculates x raised to the power n (x^n).

module MyPow
  (
    myPow
  ) where

myPow :: Double -> Int -> Double
myPow x n = if n < 0 then 1 / a else a
  where a = myPow' x $ abs n

myPow' :: Double -> Int -> Double
myPow' _ 0 = 1
myPow' x n = x * myPow' x (n - 1)
