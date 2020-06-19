-- Given a string array words, find the maximum value of length(word[i]) * length(word[j]) where the two words do not share common letters. You may assume that each word will contain only lower case letters. If no such two words exist, return 0.

module MaxProduct2
  (
    maxProduct
  ) where

maxProduct :: [String] -> Int
maxProduct [] = 0
maxProduct xs = let ys = [ (length x, x) | x <- xs]
                in  maximum $ product' <$> ys <*> ys
  where product' :: (Int, String) -> (Int, String) -> Int
        product' (a, xs) (b, ys)
          | any (`elem` ys) xs = 0
          | otherwise = a * b
