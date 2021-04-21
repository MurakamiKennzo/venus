-- Given a string s which consists of lowercase or uppercase letters, return the length of the longest palindrome that can be built with those letters.

-- Letters are case sensitive, for example, "Aa" is not considered a palindrome here.

module LongestPalindrome
  (
    longestPalindrome
  ) where

import Data.Map ( Map
                , insertWith )

longestPalindrome :: String -> Int
longestPalindrome s = let a = oddCount . lengthChar $ s
                      in  if a == 0 then length s else length s - a + 1
  where lengthChar :: String -> Map Char Int
        lengthChar = foldr (\c map -> insertWith (+) c 1 map) mempty

        oddCount :: Map Char Int -> Int
        oddCount = foldr (\a b -> b + if odd a then 1 else 0) 0
