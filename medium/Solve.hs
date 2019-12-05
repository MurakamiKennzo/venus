-- Given a 2D board containing 'X' and 'O' (the letter O), capture all regions surrounded by 'X'.

-- A region is captured by flipping all 'O's into 'X's in that surrounded region.

module Solve
  (
    solve
  ) where

import Data.Sequence ( Seq(..) 
                     , fromList
                     , adjust'
                     , update )

import Data.List ( delete )

import Data.Foldable (toList)

-- solve :: [[Char]] -> [[Char]]
solve a = flip updateXs a . indicesOfX' (indicesOfList a) . concatMap (linkIndeicesOfOs a) . findIndicesOfBorderOs $ a

findIndicesOfBorderOs :: [[Char]] -> [(Int, Int)]
findIndicesOfBorderOs [] = []
findIndicesOfBorderOs a = let b = findIndicesOfBorderOs' 0 a
                              c = zip (repeat 0) . map fst . filter (\(i, v) -> v == 'O') . zip [0 ..] . head $ a
                              d = zip (repeat (if length a - 1 < 0 then 0 else length a - 1)) . map fst . filter (\(i, v) -> v == 'O') . zip [0 ..] . last $ a
                          in  b <> c <> d

findIndicesOfBorderOs' :: Int -> [[Char]] -> [(Int, Int)]
findIndicesOfBorderOs' _ [] = []
findIndicesOfBorderOs' i (x:xs)
  | x !! 0 == 'O' = (i, 0) : a
  | x !! b == 'O' = (i, b) : a
  | otherwise = a
  where a = findIndicesOfBorderOs' (i + 1) xs
        b = if length x - 1 < 0 then 0 else length x - 1

linkIndeicesOfOs :: [[Char]] -> (Int, Int) -> [(Int, Int)]
linkIndeicesOfOs = linkIndeicesOfOs' []

linkIndeicesOfOs' :: [(Int, Int)] -> [[Char]] -> (Int, Int) -> [(Int, Int)]
linkIndeicesOfOs' _ [] _ = []
linkIndeicesOfOs' searched a (x, y)
  | x < 0 = []
  | y < 0 = []
  | x >= b = []
  | y >= c = []
  | (x, y) `elem` searched = []
  | otherwise = let d = (x, y) : searched
                    e = linkIndeicesOfOs' d a (x - 1, y) <> linkIndeicesOfOs' d a (x + 1, y) <> linkIndeicesOfOs' d a (x, y - 1) <> linkIndeicesOfOs' d a (x, y + 1)
                in  if (a !! x) !! y == 'O' then (x, y): e else []
  where b = length . head $ a
        c = length a

updateXs :: [(Int, Int)] -> [[Char]] -> [[Char]]
updateXs a b = let c = fromList . map fromList $ b
               in  toList . fmap toList $ updateXs' a c

updateXs' :: [(Int, Int)] -> Seq (Seq Char) -> Seq (Seq Char)
updateXs' [] a = a
updateXs' ((x, y):xs) a = let b = adjust' (update y 'X') x a
                          in  updateXs' xs b

indicesOfList :: [[Char]] -> [(Int, Int)]
indicesOfList = indicesOfList' 0

indicesOfList' :: Int -> [[Char]] -> [(Int, Int)]
indicesOfList' _ [] = []
indicesOfList' i (x:xs)
  | a == 0 = b
  | otherwise = c <> b
  where a = length x
        b = indicesOfList' (i + 1) xs
        c = map (\x -> (i, x)) [0 .. a - 1]

indicesOfX' :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
indicesOfX' a [] = a
indicesOfX' a (x:xs) = indicesOfX' xs (delete x a)
