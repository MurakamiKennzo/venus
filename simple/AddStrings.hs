-- Given two non-negative integers, num1 and num2 represented as string, return the sum of num1 and num2 as a string.

module AddStrings
  (
    addStrings
  ) where

import Data.Char ( intToDigit
                 , digitToInt )

addStrings :: String -> String -> String
addStrings xs ys = reverse $ addStrings' 0 (length xs - 1, xs) (length ys - 1, ys)

addStrings' :: Int -> (Int, String) -> (Int, String) -> String
addStrings' n (i, xs) (j, ys) 
  | i < 0 && j < 0 = if n == 0 then [] else [intToDigit n]
  | otherwise = let (a', b') = (a + b + n) `divMod` 10
                in  intToDigit b': addStrings' a' (i - 1, xs) (j - 1, ys) 
  where a = if i < 0 then 0 else digitToInt $ xs !! i
        b = if j < 0 then 0 else digitToInt $ ys !! j

type Carry = Int
