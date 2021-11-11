-- In the "100 game" two players take turns adding, to a running total, any integer from 1 to 10. The player who first causes the running total to reach or exceed 100 wins.

-- What if we change the game so that players cannot re-use integers?

-- For example, two players might take turns drawing from a common pool of numbers from 1 to 15 without replacement until they reach a total >= 100.

-- Given two integers maxChoosableInteger and desiredTotal, return true if the first player to move can force a win, otherwise, return false. Assume both players play optimally.

module CanIWin (
  canIWin
) where

import Data.Bits ( (.&.)
                 , shiftL
                 , (.|.) )

canIWin :: Int -> Int -> Bool
canIWin maxChoosableInteger desiredTotal
  | maxChoosableInteger >= desiredTotal = True
  | sum [1 .. maxChoosableInteger] < desiredTotal = False
  | otherwise = choose 0 desiredTotal maxChoosableInteger [1 .. maxChoosableInteger]

choose :: Int -> Int -> Int -> [Int] -> Bool
choose state desiredTotal _ [] = False
choose state desiredTotal maxChoosableInteger (x:xs)
  | cur .&. state /= 0 = choose state desiredTotal maxChoosableInteger xs
  | x >= desiredTotal = True
  | not $ choose (state .|. cur) (desiredTotal - x) maxChoosableInteger [1 .. maxChoosableInteger] = True
  | otherwise = choose state desiredTotal maxChoosableInteger xs
  where cur = 1 `shiftL` (x - 1)
