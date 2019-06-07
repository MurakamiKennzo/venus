-- Implement strStr().

-- Return the index of the first occurrence of needle in haystack, or -1 if needle is not part of haystack.

module StrStr
  (
    strStr
  ) where

strStr :: String -> String -> Int
strStr a b = format $ strStr' a b
  where format :: Int -> Int
        format x
          | x < 0 = - 1
          | otherwise = x - 1

strStr' :: String -> String -> Int
strStr' a b 
  | c < d = minBound :: Int
  | e == b = 1
  | otherwise = 1 + strStr' (drop 1 a) b
  where c = length a
        d = length b
        e = take d a
