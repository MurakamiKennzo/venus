module HammingDistance
  (
    hammingDistance
  ) where

import Data.Bits ( popCount
                 , xor )

hammingDistance :: Int -> Int -> Int
hammingDistance x y = popCount $ x `xor` y
