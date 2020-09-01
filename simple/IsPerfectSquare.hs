-- Given a positive integer num, write a function which returns True if num is a perfect square else False.

-- Follow up: Do not use any built-in library function such as sqrt.

module IsPerfectSquare
  (
    isPerfectSquare
  ) where

isPerfectSquare :: Int -> Bool
isPerfectSquare = isPerfectSquare' 1 

isPerfectSquare' :: Seed -> Int -> Bool
isPerfectSquare' seed n
  | n < 0 = False
  | n == 0 = True
  | otherwise = isPerfectSquare' (seed + 2) (n - seed)

type Seed = Int
