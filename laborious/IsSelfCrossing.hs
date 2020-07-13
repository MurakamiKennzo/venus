-- You are given an array x of n positive numbers. You start at point (0,0) and moves x[0] metres to the north, then x[1] metres to the west, x[2] metres to the south, x[3] metres to the east and so on. In other words, after each move your direction changes counter-clockwise.

-- Write a one-pass algorithm with O(1) extra space to determine, if your path crosses itself, or not.

module IsSelfCrossing
  (
    isSelfCrossing
  ) where

isSelfCrossing :: (Num a, Ord a) => [a] -> Bool
isSelfCrossing xs
  | length xs < 4 = False
  | otherwise = isSelfCrossing' xs
  where isSelfCrossing' :: (Num a, Ord a) => [a] -> Bool
        isSelfCrossing' [a, b, c, d] = c <= a && d >= b
        isSelfCrossing' [a, b, c, d, e] = isSelfCrossing' [a, b, c, d] ||
                                            isSelfCrossing' [b, c, d, e] ||
                                            (e == b && (e + a) >= c)
        isSelfCrossing' [a, b, c, d, e, f] = isSelfCrossing' [a, b, c, d] ||
                                              isSelfCrossing' [b, c, d, e] ||
                                              isSelfCrossing' [c, d, e, f] ||
                                              isSelfCrossing' [a, b, c, d, e] ||
                                              isSelfCrossing' [b, c, d, e, f] ||
                                              (a >= 0 && (f + b) >= d && (d + a) >= c && d > b && c > e)
        isSelfCrossing' a@(_:xs) = isSelfCrossing' (take 6 xs) || isSelfCrossing' xs
