-- Given a 32-bit signed integer, reverse digits of an integer.

module Reverse
  (
    reverseInteger
  ) where

reverseInteger :: Integer -> Integer
reverseInteger x = a . read . reverse . drop b . show $ x
  where a = if x < 0 then (0 -) else (0 +)
        b = if x < 0 then 1 else 0
