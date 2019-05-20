-- Implement atoi which converts a string to an integer.

-- The function first discards as many whitespace characters as necessary until the first non-whitespace character is found. Then, starting from this character, takes an optional initial plus or minus sign followed by as many numerical digits as possible, and interprets them as a numerical value.

-- The string can contain additional characters after those that form the integral number, which are ignored and have no effect on the behavior of this function.

-- If the first sequence of non-whitespace characters in str is not a valid integral number, or if no such sequence exists because either str is empty or it contains only whitespace characters, no conversion is performed.

-- If no valid conversion could be performed, a zero value is returned.

module MyAtoi
  (
    myAtoi
  ) where

import Data.Char (isDigit)

myAtoi :: String -> Int
myAtoi a = case dropWhile (' ' ==) a of
  [] -> 0
  (x:xs) -> myAtoi' x . takeWhile isDigit $ xs

myAtoi' :: Char -> String -> Int
myAtoi' a []
  | isDigit a = read [a] 
  | otherwise = 0

myAtoi' a xs
  | isDigit a || ('-' == a) = read $ a:xs
  | ('+' == a) = read $ xs
  | otherwise = 0
