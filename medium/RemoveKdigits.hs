-- Given a non-negative integer num represented as a string, remove k digits from the number so that the new number is the smallest possible.

-- Note:
-- The length of num is less than 10002 and will be ≥ k.
-- The given num does not contain any leading zero.

module RemoveKdigits
  (
    removeKdigits
  ) where

removeKdigits :: String -> Int -> String
removeKdigits xs k = case removeKdigits' xs k of
                        "" -> "0"
                        xs' -> xs'

removeKdigits' :: String -> Int -> String
removeKdigits' xs 0 = xs
removeKdigits' xs n = removeKdigits' (dropWhile (== '0') . removeKdigits'' $ xs) (n - 1)

removeKdigits'' :: String -> String
removeKdigits'' "" = ""
removeKdigits'' [_] = ""
removeKdigits'' (x:y:xs)
  | (read [x] :: Int) > read [y] = y:xs
  | otherwise = x: removeKdigits'' (y:xs)
