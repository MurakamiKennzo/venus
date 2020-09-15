-- Calculate the sum of two integers a and b, but you are not allowed to use the operator + and -.

module GetSum
  (
    getSum
  ) where

import Data.Bits ( (.&.)
                 , xor
                 , shiftL )

getSum :: Int -> Int -> Int
getSum a b = getSum' (a `xor` b) (a .&. b)

getSum' :: Sum -> Carry -> Int
getSum' n 0 = n
getSum' n c = getSum n (c `shiftL` 1)

type Carry = Int
type Sum = Int
