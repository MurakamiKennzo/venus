-- Given a string, find the first non-repeating character in it and return its index. If it doesn't exist, return -1.

module FirstUniqChar
  (
    firstUniqChar
  ) where

firstUniqChar :: String -> Int
firstUniqChar = firstUniqChar' 0 ""

firstUniqChar' :: Int -> String -> String -> Int
firstUniqChar' n _ "" = n
firstUniqChar' n xs (y:ys)
  | y `elem` xs || y `elem` ys = firstUniqChar' (succ n) (y:xs) ys
  | otherwise = n
