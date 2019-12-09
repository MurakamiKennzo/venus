-- There are N gas stations along a circular route, where the amount of gas at station i is gas[i].

-- You have a car with an unlimited gas tank and it costs cost[i] of gas to travel from station i to its next station (i+1). You begin the journey with an empty tank at one of the gas stations.

-- Return the starting gas station's index if you can travel around the circuit once in the clockwise direction, otherwise return -1.

-- Note:

-- If there exists a solution, it is guaranteed to be unique.
-- Both input arrays are non-empty and have the same length.
-- Each element in the input arrays is a non-negative integer.

module CanCompleteCircuit
  (
    canCompleteCircuit
  ) where

import Data.List ( findIndex 
                 , inits
                 , tails )

canCompleteCircuit :: [Int] -> [Int] -> Int
canCompleteCircuit a b = let c = allConnect $ combine a b
                             d = map (map (flip run)) $ c
                             e = map (foldl (>>=) (return 0)) d
                             f = findIndex ( /= Nothing ) e
                         in  maybe (-1) id f


combine :: [Int] -> [Int] -> [Int]
combine [] [] = []
combine (x:xs) (y:ys) = (x - y) : combine xs ys

allConnect :: [Int] -> [[Int]]
allConnect a = let b = inits a
                   c = tails a
               in  zipWith (++) c b

run :: Int -> Int -> Maybe Int
run a b = if a + b < 0 then Nothing else Just (a + b)
