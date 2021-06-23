-- Given two integers n and k, return the kth lexicographically smallest integer in the range [1, n].

module FindKthNumber
  (
    findKthNumber
  ) where

findKthNumber :: Int -> Int -> Int
findKthNumber n k = findKthNumber' n k 1
  where findKthNumber' :: Int -> Int -> Int -> Int
        findKthNumber' n k prefix
          | k == 1 = prefix
          | k <= count = findKthNumber' n (k - 1) (prefix * 10)
          | otherwise = findKthNumber' n (k - count) (prefix + 1)
          where count = countOfPrefix n prefix

countOfPrefix :: Int -> Int -> Int
countOfPrefix n p = countOfPrefix' n (p, p + 1)
  where countOfPrefix' :: Int -> (Int, Int) -> Int
        countOfPrefix' n (cur, next)
          | cur > n = 0
          | otherwise = min (n + 1) next - cur + countOfPrefix' n (cur * 10, next * 10)
