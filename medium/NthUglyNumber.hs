-- Write a program to find the n-th ugly number.

-- Ugly numbers are positive numbers whose prime factors only include 2, 3, 5. 

module NthUglyNumber
  (
    nthUglyNumber
  ) where

import Data.List ( sort 
                 , nub )

nthUglyNumber :: Int -> Int
nthUglyNumber = last . flip take uglyNumbers

uglyNumbers :: [Int]
uglyNumbers = 1:uglyNumbers' [2, 3, 5]

uglyNumbers' :: [Int] -> [Int]
uglyNumbers' xs = let xs' = nub . sort $ xs
                      a = minimum xs'
                      xs'' = tail xs' <> [a * 2, a * 3, a * 5]
                  in  a:uglyNumbers' xs''
