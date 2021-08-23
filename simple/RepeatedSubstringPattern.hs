-- Given a string s, check if it can be constructed by taking a substring of it and appending multiple copies of the substring together.

module RepeatedSubstringPattern
  (
    repeatedSubstringPattern
  ) where

repeatedSubstringPattern :: String -> Bool
repeatedSubstringPattern xs = repeatedSubstringPattern' (1, length xs) xs

repeatedSubstringPattern' :: (Int, Int) -> String -> Bool
repeatedSubstringPattern' (i, l) xs
  | i * 2 > l = False
  | l `mod` i == 0 = if repeatedSubstringPattern'' (i, i, l) xs then True else a
  | otherwise = a
  where a = repeatedSubstringPattern' (i + 1, l) xs

repeatedSubstringPattern'' :: (Int, Int, Int) -> String -> Bool
repeatedSubstringPattern'' (i, i', l) xs
  | i >= l = True
  | otherwise = if xs !! i /= xs !! (i - i') then False else repeatedSubstringPattern'' (i + 1, i', l) xs
