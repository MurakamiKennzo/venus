-- Given two integers representing the numerator and denominator of a fraction, return the fraction in string format.

-- If the fractional part is repeating, enclose the repeating part in parentheses.

module FractionToDecimal
  (
    fractionToDecimal
  ) where

import Data.Char ( intToDigit )
import Data.List ( elemIndex )

fractionToDecimal :: Int -> Int -> Decimal
fractionToDecimal m n = let (a, b) = m `divMod` n
                        in  if b == 0 then show a else show a <> "." <> decimal b n []

decimal :: Int -> Int -> [Int] -> String
decimal a b c
  | e == 0 = map intToDigit . (++ [d]) $ c
  | d `elem` c = case d `elemIndex` c of
                  Nothing -> ""
                  Just i -> let (f, g) = splitAt i . map intToDigit $ c
                            in  f ++ "(" ++ g ++ ")"
  | otherwise = decimal e b (c ++ [d])
  where (d, e) = (a * 10) `divMod` b

type Decimal = String
