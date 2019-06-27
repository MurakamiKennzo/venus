-- Given two non-negative integers num1 and num2 represented as strings, return the product of num1 and num2, also represented as a string.

module Multiply
  (
    multiply
  ) where

multiply :: String -> String -> String
multiply a b = show $ read a * read b
