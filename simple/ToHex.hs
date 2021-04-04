-- Given an integer, write an algorithm to convert it to hexadecimal. For negative integer, two’s complement method is used.

-- Note:

-- All letters in hexadecimal (a-f) must be in lowercase.
-- The hexadecimal string must not contain extra leading 0s. If the number is zero, it is represented by a single zero character '0'; otherwise, the first character in the hexadecimal string will not be the zero character.
-- The given number is guaranteed to fit within the range of a 32-bit signed integer.
-- You must not use any method provided by the library which converts/formats the number to hex directly.

module ToHex
  (
    toHex
  ) where

import Data.Bits ( (.&.)
                 , shiftR )

toHex :: Int -> String
toHex = reverse . toHex'

toHex' :: Int -> String
toHex' n
  | y == 0 = [z]
  | otherwise = take 8 $ z: toHex' y
  where x = n .&. 15
        y = n `shiftR` 4
        z = (['0' .. '9'] <> ['a' .. 'f']) !! x
