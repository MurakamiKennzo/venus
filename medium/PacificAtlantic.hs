-- You are given an m x n integer matrix heights representing the height of each unit cell in a continent. The Pacific ocean touches the continent's left and top edges, and the Atlantic ocean touches the continent's right and bottom edges.

-- Water can only flow in four directions: up, down, left, and right. Water flows from a cell to an adjacent one with an equal or lower height.

-- Return a list of grid coordinates where water can flow to both the Pacific and Atlantic oceans.

module PacificAtlantic
  (
    pacificAtlantic
  ) where

import Control.Monad ( guard )

pacificAtlantic :: Matrix Int -> [(Int, Int)]
pacificAtlantic [] = []
pacificAtlantic a@(x:_) = do
  m <- [0 .. row - 1]
  n <- [0 .. col - 1]
  guard $ pacificAtlantic' (m, n) (row, col) a
  return (m, n)
  where row = length a
        col = length x

pacificAtlantic' :: PacificAtlantic'
pacificAtlantic' a b xs = pacific a b xs && atlantic a b xs

pacific :: PacificAtlantic'
pacific (m, n) (row, col) xs
  | m == 0 || n == 0 = True
  | otherwise = or [ topflow pacific (m, n) (row, col) xs
                   , rightflow pacific (m, n) (row, col) xs
                   , bottomflow pacific (m, n) (row, col) xs
                   , leftflow pacific (m, n) (row, col) xs ]

atlantic :: PacificAtlantic'
atlantic (m, n) (row, col) xs
  | m == row - 1 || n == col - 1 = True
  | otherwise = or [ topflow atlantic (m, n) (row, col) xs
                   , rightflow atlantic (m, n) (row, col) xs
                   , bottomflow atlantic (m, n) (row, col) xs
                   , leftflow atlantic (m, n) (row, col) xs ]

topflow :: PacificAtlantic' -> PacificAtlantic'
topflow f (m, n) (row, col) xs
  | m <= 0 = False
  | a < b = False
  | a == -1 = False
  | otherwise = f (m - 1, n) (row, col) (updateMatrixV (m, n) xs)
  where a = xs !! m !! n
        b = xs !! (m - 1) !! n

rightflow :: PacificAtlantic' -> PacificAtlantic'
rightflow f (m, n) (row, col) xs
  | n >= col - 1 = False
  | a < b = False
  | a == -1 = False
  | otherwise = f (m, n + 1) (row, col) (updateMatrixV (m, n) xs)
  where a = xs !! m !! n
        b = xs !! m !! (n + 1)

bottomflow :: PacificAtlantic' -> PacificAtlantic'
bottomflow f (m, n) (row, col) xs
  | m >= row - 1 = False
  | a < b = False
  | a == -1 = False
  | otherwise = f (m + 1, n) (row, col) (updateMatrixV (m, n) xs)
  where a = xs !! m !! n
        b = xs !! (m + 1) !! n

leftflow :: PacificAtlantic' -> PacificAtlantic'
leftflow f (m, n) (row, col) xs
  | n <= 0 = False
  | a < b = False
  | a == -1 = False
  | otherwise = f (m, n - 1) (row, col) (updateMatrixV (m, n) xs)
  where a = xs !! m !! n
        b = xs !! m !! (n - 1)

updateMatrixV :: (Int, Int) -> Matrix Int -> Matrix Int
updateMatrixV (m, n) xs = take m xs <> [take n x <> [-1] <> drop (n + 1) x] <> (drop (m + 1) xs)
  where x = xs !! m

type PacificAtlantic' = (Int, Int) -> (Int, Int) -> Matrix Int -> Bool

type Matrix a = [[a]]
