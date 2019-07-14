-- Given a string s consists of upper/lower-case alphabets and empty space characters ' ', return the length of last word in the string.

-- If the last word does not exist, return 0.

-- Note: A word is defined as a character sequence consists of non-space characters only.

module LengthOfLastWord
  (
    lengthOfLastWord
  ) where

import Data.Char (isSpace)

lengthOfLastWord :: String -> Int
lengthOfLastWord = lengthOfLastWord' . dropWhile isSpace . reverse

lengthOfLastWord' :: String -> Int
lengthOfLastWord' [] = 0
lengthOfLastWord' (x:xs)
  | isSpace x = 0
  | otherwise = 1 + lengthOfLastWord' xs
