-- Write a function that takes a string as input and reverse only the vowels of a string.

module ReverseVowels
  (
    reverseVowels
  ) where

reverseVowels :: String -> String
reverseVowels = do
  l <- length
  s <- id
  return $ reverseVowels' 0 (l - 1) s

reverseVowels' :: Int -> Int -> String -> String
reverseVowels' l r s
  | l >= r = s
  | left `elem` vowels && right `elem` vowels = reverseVowels' (succ l) (pred r) s'
  | left `elem` vowels = reverseVowels' l (pred r) s
  | right `elem` vowels = reverseVowels' (succ l) r s
  | otherwise = reverseVowels' (succ l) (pred r) s
  where vowels = "AEIOUaeiou"
        left = s !! l
        right = s !! r
        s' = take l s <> [right] <> ( take (r - l - 1) . drop (succ l) $ s) <> [left] <> (drop (succ r) $ s)
