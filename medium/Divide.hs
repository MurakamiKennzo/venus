-- Given two integers dividend and divisor, divide two integers without using multiplication, division and mod operator.

-- Return the quotient after dividing dividend by divisor.

-- The integer division should truncate toward zero.

module Divide 
  (
    divide
  ) where

divide :: Int -> Int -> Int
divide a b = if c + d == 0 then -e else e
  where c = signum a
        d = signum b
        e = abs a `divide'` abs b
        divide' :: Int -> Int -> Int
        _ `divide'` 0 = error "divide by zero"
        a `divide'` b
          | a < b = 0
          | otherwise = 1 + (a - b) `divide'` b
