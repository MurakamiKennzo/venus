-- Given a m x n matrix, if an element is 0, set its entire row and column to 0.

module SetZeroes
  (
    setZeroes
  ) where

import Data.List ( elemIndices )

setZeroes :: (Eq a, Num a) => Matrix a -> Matrix a
setZeroes m = setZeroes' m . rowColumns $ m

rowColumns :: (Eq a, Num a) => Matrix a -> [(Int, Int)]
rowColumns = tuples . map (elemIndices 0)
  where tuples :: [[Int]] -> [(Int, Int)]
        tuples = foldl (++) [] . zipWith (zipWith (,)) (map repeat [0 ..])

setZeroes' :: (Num a) => Matrix a -> [(Int, Int)] -> Matrix a
setZeroes' m i = foldl setZero m i
  where setZero :: (Num a) => Matrix a -> (Int, Int) -> Matrix a
        setZero m (x, y) = let m' = update x (map (\_ -> 0) $ m !! x) m
                           in map (update y 0) m'

update :: Int -> a -> [a] -> [a]
update i x xs = take i xs ++ [x] ++ drop (i + 1) xs

type Matrix a = [[a]]
