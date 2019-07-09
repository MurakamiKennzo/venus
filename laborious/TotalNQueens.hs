-- The n-queens puzzle is the problem of placing n queens on an n×n chessboard such that no two queens attack each other.

-- Given an integer n, return the number of distinct solutions to the n-queens puzzle.

module TotalNQueens
  (
    totalNQueens
  ) where

import Data.List (permutations, nub)

queen :: Char
queen = 'Q'

empty :: Char
empty = '.'

type Place = (Int, Int)

totalNQueens :: Int -> Int
totalNQueens n = length . filter (flip solvedQueen n) . simpleSolveQueen $ n

simpleSolveQueen :: Int -> [[Place]]
simpleSolveQueen n = map (zip [0 .. n - 1]) . permutations $ [0 .. n - 1]

diagonal :: Place -> Int -> [Place]
diagonal (row, col) n =
  let main' = mainDiagonal (row, col) n
      converse' = converseDiagonal (row, col) n
  in nub $ main' ++ converse'

mainDiagonal :: Place -> Int -> [Place]
mainDiagonal (row, col) n = mainDiagonalLeft (row - 1, col - 1) n ++ [(row, col)] ++ mainDiagonalRight (row + 1, col + 1) n

mainDiagonalLeft :: Place -> Int -> [Place]
mainDiagonalLeft (row, col) n
  | row >= n = []
  | col >= n = []
  | row < 0 = []
  | col < 0 = []
mainDiagonalLeft (0, col) n = [(0, col)]
mainDiagonalLeft (row, 0) n = [(row, 0)]
mainDiagonalLeft (row, col) n 
  | row == n - 1 = [(row, col)]
  | col == n - 1 = [(row, col)]
  | otherwise = mainDiagonalLeft (row - 1, col - 1) n ++ [(row, col)]


mainDiagonalRight :: Place -> Int -> [Place]
mainDiagonalRight (row, col) n
  | row >= n = []
  | col >= n = []
  | row < 0 = []
  | col < 0 = []
mainDiagonalRight (0, col) n = [(0, col)]
mainDiagonalRight (row, 0) n = [(row, 0)]
mainDiagonalRight (row, col) n 
  | row == n - 1 = [(row, col)]
  | col == n - 1 = [(row, col)]
  | otherwise = (row, col) : mainDiagonalRight (row + 1, col + 1) n

converseDiagonal :: Place -> Int -> [Place]
converseDiagonal (row, col) n = converseDiagonalLeft (row - 1, col + 1) n ++ [(row, col)] ++ converseDiagonalRight (row + 1, col - 1) n

converseDiagonalLeft :: Place -> Int -> [Place]
converseDiagonalLeft (row, col) n
  | row >= n = []
  | col >= n = []
  | row < 0 = []
  | col < 0 = []
converseDiagonalLeft (0, col) n = [(0, col)]
converseDiagonalLeft (row, 0) n = [(row, 0)]
converseDiagonalLeft (row, col) n 
  | row == n - 1 = [(row, col)]
  | col == n - 1 = [(row, col)]
  | otherwise = converseDiagonalLeft (row - 1, col + 1) n ++ [(row, col)]

converseDiagonalRight :: Place -> Int -> [Place]
converseDiagonalRight (row, col) n
  | row >= n = []
  | col >= n = []
  | row < 0 = []
  | col < 0 = []
converseDiagonalRight (0, col) n = [(0, col)]
converseDiagonalRight (row, 0) n = [(row, 0)]
converseDiagonalRight (row, col) n 
  | row == n - 1 = [(row, col)]
  | col == n - 1 = [(row, col)]
  | otherwise = (row, col) : converseDiagonalRight (row + 1, col - 1) n

solvedQueen :: [Place] -> Int -> Bool
solvedQueen place n = 
  let a = map (flip diagonal n) place
      b = zip place a
      c = all (\(p, diagonals) -> [p] == filter (`elem` diagonals) place) b
  in c
