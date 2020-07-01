-- Given an integer array nums, return the number of range sums that lie in [lower, upper] inclusive.
-- Range sum S(i, j) is defined as the sum of the elements in nums between indices i and j (i ≤ j), inclusive.

module CountRangeSum
  (
    countRangeSum
  ) where

countRangeSum :: (Num a, Ord a) => [a] -> (a, a) -> Int
countRangeSum xs a = length . filter (f a) $ rangeSum xs
  where f :: (Num a, Ord a) => (a, a) -> a -> Bool
        f (l, u) = do
          x <- (>= l)
          y <- (<= u)
          return $ x && y

rangeSum :: (Num a) => [a] -> [a]
rangeSum [] = []
rangeSum (x:xs) = let a = rangeSum xs
                  in  x:map (x +) a <> a
