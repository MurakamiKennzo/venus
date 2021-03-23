-- Given a positive integer n, you can apply one of the following operations:

-- If n is even, replace n with n / 2.
-- If n is odd, replace n with either n + 1 or n - 1.
-- Return the minimum number of operations needed for n to become 1.

module IntegerReplacement
  (
    integerReplacement
  ) where

integerReplacement :: Int -> Int
integerReplacement 1 = 0
integerReplacement n
  | even n = 1 + (integerReplacement $ n `div` 2)
  | otherwise = 1 + min (integerReplacement $ succ n) (integerReplacement $ pred n)
