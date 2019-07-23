-- Given two binary strings, return their sum (also a binary string).

-- The input strings are both non-empty and contains only characters 1 or 0.

module AddBinary
  (
    addBinary
  ) where

import Data.Char ( digitToInt
                 , intToDigit )

addBinary :: String -> String -> String
addBinary a b = let c = length a - length b
                    d = replicate (abs c) '0'
                    (u, v) = if c > 0 then (a, d ++ b) else (d ++ a, b)
                in  reverse $ addBinary' '0' (reverse u) (reverse v)

addBinary' :: Carry -> String -> String -> String
addBinary' c "" "" = if c == '0' then "" else "1"
addBinary' c (x:xs) (y:ys) = let a = digitToInt x + digitToInt y + digitToInt c
                                 (u, v) = if a < 2 then (intToDigit a, '0') else (intToDigit (a - 2), '1')
                             in  u:addBinary' v xs ys

type Carry = Char
