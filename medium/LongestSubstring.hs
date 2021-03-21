-- Given a string s and an integer k, return the length of the longest substring of s such that the frequency of each character in this substring is greater than or equal to k.

module LongestSubstring
  (
    longestSubstring
  ) where

import qualified Data.Map as Map

longestSubstring :: String -> Int -> Int
longestSubstring s n = maybe (length s) id $ do
  (c, _) <- Map.lookupMin . Map.filter (< n) . stringMap $ s
  return $ maximum . map (flip longestSubstring n). splitWith c $ s

stringMap :: String -> Map.Map Char Int
stringMap = foldr (\c map -> Map.insertWith (+) c 1 map) Map.empty

splitWith :: Char -> String -> [String]
splitWith c s = let (s', s'') = break (== c) s 
                in  if null s'' then [s'] else s': splitWith c (tail s'')
