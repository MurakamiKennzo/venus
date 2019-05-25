-- Given an integer, convert it to a roman numeral. Input is guaranteed to be within the range from 1 to 3999.

module IntToRoman
  (
    intToRoman
  ) where

data Roman = I | IV | V | IX | X | XL | L | XC | C | CD | D | CM | M deriving (Show, Eq)

intToRoman :: Int -> String
intToRoman = toString . intToRoman'

intToRoman' :: Int -> [Roman]
intToRoman' 0 = []
intToRoman' a
  | a > 1000 = (M:) $ intToRoman' $ a - 1000
  | a >= 900 = (CM:) $ intToRoman' $ a - 900
  | a >= 500 = (D:) $ intToRoman' $ a - 500
  | a >= 400 = (CD:) $ intToRoman' $ a - 400
  | a >= 100 = (C:) $ intToRoman' $ a - 100
  | a >= 90 = (XC:) $ intToRoman' $ a - 90
  | a >= 50 = (L:) $ intToRoman' $ a - 50
  | a >= 40 = (XL:) $ intToRoman' $ a - 40
  | a >= 10 = (X:) $ intToRoman' $ a - 10
  | a >= 9 = (IX:) $ intToRoman' $ a - 9
  | a >= 5 = (V:) $ intToRoman' $ a - 5
  | a >= 4 = (IV:) $ intToRoman' $ a - 4
  | otherwise = (I:) $ intToRoman' $ a - 1

toString :: [Roman] -> String
toString [] = []
toString (x:xs) = show x ++ toString xs
