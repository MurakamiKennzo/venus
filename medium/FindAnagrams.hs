-- Given two strings s and p, return an array of all the start indices of p's anagrams in s. You may return the answer in any order.

module FindAnagrams
  (
    findAnagrams
  ) where

import qualified Data.Map as Map

findAnagrams :: String -> String -> [Int]
findAnagrams s p = let sl = length s
                       pl = length p
                       m = buildCharMap p
                       n = buildCharMap $ take pl s
                   in findAnagrams' (0, pl - 1) (sl - 1) (n, m) s

buildCharMap :: String -> CharMap
buildCharMap = foldr (\c m -> Map.insertWith (+) c 1 m) mempty

findAnagrams' :: (Int, Int) -> Int -> (CharMap, CharMap) -> String -> [Int]
findAnagrams' (s, e) n (m', m) xs 
  | e > n = []
  | m' == m = s: findAnagrams' (s + 1, e + 1) n (m'', m) xs
  | otherwise = findAnagrams' (s + 1, e + 1) n (m'', m) xs
  where m'' = Map.insertWith (+) (xs !! (e + 1)) 1 . Map.update (\a -> if a == 1 then Nothing else return $ a - 1) (xs !! s) $ m'

type CharMap = Map.Map Char Int
