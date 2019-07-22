-- Given a non-empty array of digits representing a non-negative integer, plus one to the integer.

-- The digits are stored such that the most significant digit is at the head of the list, and each element in the array contain a single digit.

-- You may assume the integer does not contain any leading zero, except the number 0 itself.

module PlusOne
  (
    plusOne
  ) where

plusOne :: [Int] -> [Int]
plusOne = reverse . plusOne' 1 . reverse

plusOne' :: Carry -> [Int] -> [Int]
plusOne' x [] = if x == 1 then [1] else []
plusOne' x (y:ys) = let z = x + y
                        (a, b) = if z < 10 then (z, 0) else (z - 10, 1)
                    in  a : plusOne' b ys

type Carry = Int
