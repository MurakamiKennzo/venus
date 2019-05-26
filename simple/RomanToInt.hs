-- Given a roman numeral, convert it to an integer. Input is guaranteed to be within the range from 1 to 3999.

module RomanToInt
  (
    romanToInt
  ) where

data Roman = I 
          | IV 
          | V 
          | IX 
          | X 
          | XL 
          | L 
          | XC 
          | C 
          | CD 
          | D 
          | CM 
          | M deriving (Show, Eq, Ord, Read)

romanToInt :: String -> Int
romanToInt = foldl1 (+) . map toInt . romanToInt'

romanToInt' :: String -> [Roman]
romanToInt' [] = []
romanToInt' [a] = [read [a]]
romanToInt' (x:y:z)
  | a < b = read [x, y] : romanToInt' z
  | otherwise = read [x] : romanToInt' (y:z)
  where a = read [x] :: Roman
        b = read [y] :: Roman

toInt :: Roman -> Int
toInt I = 1
toInt IV = 4
toInt V = 5
toInt IX = 9
toInt X = 10
toInt XL = 40
toInt L = 50
toInt XC = 90
toInt C = 100
toInt CD = 400
toInt D = 500
toInt CM = 900
toInt M = 1000
