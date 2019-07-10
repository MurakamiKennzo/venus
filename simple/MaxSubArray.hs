-- Given an integer array nums, find the contiguous subarray (containing at least one number) which has the largest sum and return its sum.

module MaxSubArray
  (
    maxSubArray
  ) where

maxSubArray :: [Int] -> Int
maxSubArray a = getMInt . maxSubArray' Empty . map MInt $ a

maxSubArray' :: MInt -> [MInt] -> MInt
maxSubArray' a [] = a
maxSubArray' a [b] = a +> if b >= zero then b else zero
maxSubArray' a (x:y:z)
  | x >= zero = maxSubArray' (a +> x) (y:z)
  | otherwise = let (u, v) = if x +> y >= zero
                              then (maxSubArray' (a +> x +> y) z, maxSubArray' y z)
                              else (a, maxSubArray' y z)
                in max u v

data MInt = Empty | MInt Int deriving (Eq)

instance Ord MInt where
  Empty <= Empty = True
  Empty <= _ = True
  _ <= Empty = False
  MInt a <= MInt b = a <= b

zero :: MInt
zero = MInt 0

infixl 6 +>

(+>) :: MInt -> MInt ->MInt
Empty +> a = a
(MInt a) +> Empty = MInt a
(MInt a) +> (MInt b) = MInt (a + b)

getMInt :: MInt -> Int
getMInt Empty = minBound :: Int
getMInt (MInt a) = a
