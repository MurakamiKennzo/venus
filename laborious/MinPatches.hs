-- Given a sorted positive integer array nums and an integer n, add/patch elements to the array such that any number in range [1, n] inclusive can be formed by the sum of some elements in the array. Return the minimum number of patches required.

module MinPatches
  (
    minPatches
  ) where

import Data.List ( nub
                 , delete
                 , find
                 , minimumBy )
import Data.Function ( on )                 

minPatches :: [Int] -> Int -> Int
minPatches xs n = ((-) `on` length) ys xs 
  where ys = minPatches' n xs

minPatches' :: Int -> [Int] -> [Int]
minPatches' n xs = let a = patch n ([1 .. n], xs)
                   in  minimumBy (compare `on` length) a

patch :: Int -> ([Int], [Int]) -> [[Int]]
patch n (xs, ys) = maybe [ys] (foldMap $ patch n) $ do
    b <- find (`notElem` a) [1 .. n]
    let c = b:map (b -) a
        d = filter (`elem` xs) c
    return $ map (\e -> (delete e xs, e:ys)) d
  where a = sum' ys

sum' :: (Eq a, Num a) => [a] -> [a]
sum' [] = []
sum' [x] = [x]
sum' (x:xs) = let ys = sum' xs
              in  nub $ x:map (+ x) ys <> ys
