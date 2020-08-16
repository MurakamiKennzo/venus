-- You have a number of envelopes with widths and heights given as a pair of integers (w, h). One envelope can fit into another if and only if both the width and height of one envelope is greater than the width and height of the other envelope.

-- What is the maximum number of envelopes can you Russian doll? (put one inside other)

-- Note:
-- Rotation is not allowed.

module MaxEnvelopes
  (
    maxEnvelopes
  ) where

import Data.List ( sort )

maxEnvelopes :: [(Int, Int)] -> Int
maxEnvelopes = maxEnvelopes' . sort . map Envelop

maxEnvelopes' :: [Envelop] -> Int
maxEnvelopes' [] = 0
maxEnvelopes' [a] = 1
maxEnvelopes' (x:y:xs)
  | y > x = 1 + maxEnvelopes' (y:xs)
  | otherwise = max (maxEnvelopes' (x:xs)) (maxEnvelopes' (y:xs))

newtype Envelop = Envelop { getEnvelop :: (Int, Int) } deriving (Show, Eq)

instance Ord Envelop where
  (Envelop (x, y)) `compare` (Envelop (x', y'))
    | x > x' && y > y' = GT
    | x == x' && y == y' = EQ
    | otherwise = LT
