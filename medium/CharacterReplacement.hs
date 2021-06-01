-- You are given a string s and an integer k. You can choose any character of the string and change it to any other uppercase English character. You can perform this operation at most k times.

-- Return the length of the longest substring containing the same letter you can get after performing the above operations.

module CharacterReplacement
  (
    characterReplacement
  ) where

import qualified Data.Map as Map

characterReplacement :: String -> Int -> Int
characterReplacement xs k = characterReplacement' (0, mempty, 0, 0) (length xs, xs) k

characterReplacement' :: (Int, Map.Map Char Int, Int, Int) -> (Int, String) -> Int -> Int
characterReplacement' (n, map, l, r) (len, xs) k
  | r >= len = r - l
  | otherwise = characterReplacement' (n', map'', l', r') (len, xs) k
  where map' = Map.insertWith (+) (xs !! r) 1 map
        n' = max n $ map' Map.! (xs !! r)
        b = r - l + 1 > n' + k
        map'' = if b then Map.adjust (subtract 1) (xs !! l) map' else map'
        l' = if b then succ l else l
        r' = succ r
