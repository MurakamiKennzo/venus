-- Write a program to find the nth super ugly number.

-- Super ugly numbers are positive numbers whose all prime factors are in the given prime list primes of size k.

module NthSuperUglyNumber
  (
    nthSuperUglyNumber
  ) where

nthSuperUglyNumber :: (Integral a) => Int -> [a] -> a
nthSuperUglyNumber n xs = superUglyNumber xs !! (pred n)

superUglyNumber :: (Integral a) => [a] -> [a]
superUglyNumber xs = 1: filter (superUglyNumber' xs) [2 ..]
  where superUglyNumber' :: (Integral a) => [a] -> a -> Bool
        superUglyNumber' xs a
          | null xs = False
          | a `elem` xs = True
          | otherwise = any (superUglyNumber' xs) . map fst . filter ((== 0) . snd) . map (a `divMod`) $ xs
