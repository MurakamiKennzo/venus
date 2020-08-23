-- Given a non-negative integer n, count all numbers with unique digits, x, where 0 ≤ x < 10n.

module CountNumbersWithUniqueDigits
  (
    countNumbersWithUniqueDigits
  ) where

countNumbersWithUniqueDigits :: Int -> Int
countNumbersWithUniqueDigits n = foldr (+) 0 . map countNumbersWithUniqueDigits' $ [1 .. n]

countNumbersWithUniqueDigits' :: Int -> Int
countNumbersWithUniqueDigits' 1 = 10
countNumbersWithUniqueDigits' n = 9 * arrangement 9 (n - 1)

arrangement :: Int -> Int -> Int
arrangement n 1 = n
arrangement n i = n * arrangement (n - 1) (i - 1)
