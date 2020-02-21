-- Count the number of prime numbers less than a non-negative number, n.

module CountPrimes
  (
    countPrimes
  ) where

countPrimes :: Int -> Int
countPrimes n = length . takeWhile (< n) $ primes

primes :: [Int]
primes = filterPrime [2 ..]
  where filterPrime (x:xs) = x : filterPrime [ y | y <- xs, y `mod` x /= 0 ]
