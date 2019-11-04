-- Given a non-negative index k where k ≤ 33, return the kth index row of the Pascal's triangle.

-- Note that the row index starts from 0.

-- In Pascal's triangle, each number is the sum of the two numbers directly above it.

module GetRow
  (
    getRow
  ) where

getRow :: Int -> [Int]
getRow 0 = [1]
getRow 1 = [1, 1]
getRow a = let b = getRow (a - 1) in 1 : getRow' b
  where getRow' :: [Int] -> [Int]
        getRow' [] = [1]
        getRow' [_] = [1]
        getRow' (a:c@(b:_)) = a + b : getRow' c
