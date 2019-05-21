-- Determine whether an integer is a palindrome. An integer is a palindrome when it reads the same backward as forward.

module IsPalindrome
  (
    isPalindrome
  ) where

isPalindrome :: Int -> Bool
isPalindrome 0 = True
isPalindrome x 
  | x < 0 = False
  | x `mod` 10 == 0 = False
  | otherwise = isPalindrome' 0 x

isPalindrome' :: Int -> Int -> Bool
isPalindrome' a b
  | a > b = a `div` 10 == b
  | a == b = True
  | otherwise =
    let (c, d) = b `divMod` 10
    in  isPalindrome' (a * 10 + d) c
