-- You are given a string s, return the number of segments in the string. 

-- A segment is defined to be a contiguous sequence of non-space characters.

module CountSegments
  (
    countSegments
  ) where

countSegments :: String -> Int
countSegments [] = 0
countSegments (x:xs)
  | x /= ' ' = 1 + countSegments' xs
  | otherwise = countSegments xs

countSegments' :: String -> Int
countSegments' [] = 0
countSegments' [_] = 0
countSegments' (x:y:xs)
  | x == ' ' && y /= ' ' = 1 + countSegments' xs
  | otherwise = countSegments' (y:xs)
