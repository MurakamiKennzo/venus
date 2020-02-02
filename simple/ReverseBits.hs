-- Reverse bits of a given 32 bits unsigned integer.

module ReverseBits
  (
    reverseBits
  ) where

reverseBits :: Int -> Int
reverseBits = toInt . fit32List 0 . toReverseBinary

type Binary = [Int]

fit32List :: a -> [a] -> [a]
fit32List x xs = let a = length xs
                 in  if a == 32 then xs else xs <> take (32 - a) (repeat x)

toReverseBinary :: Int -> Binary
toReverseBinary 0 = []
toReverseBinary a
  | b == 0 = [c]
  | otherwise = c : toReverseBinary b
  where (b, c) = a `divMod` 2

toInt :: Binary -> Int
toInt [] = 0
toInt (x:xs) = x * 2 ^ length xs + toInt xs
