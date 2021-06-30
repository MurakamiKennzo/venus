-- Given an array of characters chars, compress it using the following algorithm:

-- Begin with an empty string s. For each group of consecutive repeating characters in chars:

-- If the group's length is 1, append the character to s.
-- Otherwise, append the character followed by the group's length.
-- The compressed string s should not be returned separately, but instead be stored in the input character array chars. Note that group lengths that are 10 or longer will be split into multiple characters in chars.

-- After you are done modifying the input array, return the new length of the array.

-- You must write an algorithm that uses only constant extra space.

module Compress
  (
    compress
  ) where

compress :: String -> Int
compress "" = 0
compress (x:xs) = c + compress b
  where (a, b) = countOfChar x xs
        a' = length . show $ a
        c = if a == 0 then 1 else a' + 1

countOfChar :: Char -> String -> (Int, String)
countOfChar x [] = (0, "")
countOfChar x (y:ys)
  | x == y = let (a, b) = countOfChar x ys
             in  (1 + a, b)
  | otherwise = (0, y:ys)
