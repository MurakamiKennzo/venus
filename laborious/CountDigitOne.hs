-- Given an integer n, count the total number of digit 1 appearing in all non-negative integers less than or equal to n.

module CountDigitOne
  (
    countDigitOne
  ) where

countDigitOne :: Int -> Int
countDigitOne n
  | n == 0 = 0
  | otherwise = countDigitOne (pred n) + (length . filter (== '1') . show $ n)
