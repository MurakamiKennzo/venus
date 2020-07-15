-- Given a list of unique words, find all pairs of distinct indices (i, j) in the given list, so that the concatenation of the two words, i.e. words[i] + words[j] is a palindrome.

module PalindromePairs
  (
    palindromePairs
  ) where

palindromePairs :: [String] -> [(Int, Int)]
palindromePairs xs = map fst . filter palindrome $ combine <$> xs' <*> xs'
  where xs' = zip [0 ..] xs
        combine :: (Int, String) -> (Int, String) -> ((Int, Int), String)
        combine (i, xs) (j, ys) = ((i, j), xs <> ys)

        palindrome :: ((Int, Int), String) -> Bool
        palindrome ((i, j), xs) = i /= j && reverse xs == xs
