-- Given an integer n, return the nth digit of the infinite integer sequence [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, ...].

module FindNthDigit
  (
    findNthDigit
  ) where

findNthDigit :: Int -> Int
findNthDigit = findNthDigit' 0

findNthDigit' :: Indicator -> Int -> Int
findNthDigit' i n
  | i == 0 && n <= 9 = n
  | n > n' = findNthDigit' (succ i) (n - n')
  | otherwise = let (a, b) = n `divMod` (i + 1)
                    c = 10 ^ i + a
                    d = c - 1
                in  read . return $ if b == 0 then last . show $ d else (!! (pred b)) . show $ c
  where n' = 9 * 10 ^ i * (i + 1)

type Indicator = Int
