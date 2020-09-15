-- Your task is to calculate ab mod 1337 where a is a positive integer and b is an extremely large positive integer given in the form of an array.

module SuperPow
  (
    superPow
  ) where

superPow :: Int -> [Int] -> Int
superPow n [] = 1
superPow n xs = (a * pow b 10) `mod` 1337
  where xs' = init xs
        x = last xs
        a = pow n x
        b = superPow n xs'

pow :: Int -> Int -> Int
pow a 0 = 1
pow a n = (a `mod` 1337) * (pow a (pred n) `mod` 1337) `mod` 1337
