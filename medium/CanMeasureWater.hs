-- You are given two jugs with capacities x and y litres. There is an infinite amount of water supply available. You need to determine whether it is possible to measure exactly z litres using these two jugs.

-- If z liters of water is measurable, you must have z liters of water contained within one or both buckets by the end.

module CanMeasureWater
  (
    canMeasureWater
  ) where

canMeasureWater :: (Integral a, Ord a) => a -> a -> a -> Bool
canMeasureWater x y z
  | z > x + y = False
  | z == x + y = True
  | otherwise = if z `mod` (gcd x y) == 0 then True else False
