-- You are given row x col grid representing a map where grid[i][j] = 1 represents land and grid[i][j] = 0 represents water.

-- Grid cells are connected horizontally/vertically (not diagonally). The grid is completely surrounded by water, and there is exactly one island (i.e., one or more connected land cells).

-- The island doesn't have "lakes", meaning the water inside isn't connected to the water around the island. One cell is a square with side length 1. The grid is rectangular, width and height don't exceed 100. Determine the perimeter of the island.

module IslandPerimeter
  (
    islandPerimeter
  ) where

islandPerimeter :: Grid -> Int
islandPerimeter [] = 0
islandPerimeter a@(x:xs) = islandPerimeter' (0, 0) (length a, length x) a

islandPerimeter' :: (Int, Int) -> (Int, Int) -> Grid -> Int
islandPerimeter' (r, c) (r', c') xs 
  | r >= r'= 0
  | r < r' && c >= c' = islandPerimeter' (r + 1, 0) (r', c') xs
  | otherwise = let top = if r == 0 then 0 else if isLand $ xs !! (r - 1) !! c then 1 else 0
                    right = if c == (c' - 1) then 0 else if isLand $ xs !! r !! (c + 1) then 1 else 0
                    bottom = if r == (r' - 1) then 0 else if isLand $ xs !! (r + 1) !! c then 1 else 0
                    left = if c == 0 then 0 else if isLand $ xs !! r !! (c - 1) then 1 else 0
                    center = if isLand $ xs !! r !! c then 4 else 0
                    a = islandPerimeter' (r, c + 1) (r', c') xs
                in  a + center - if isLand $ xs !! r !! c then top + right + bottom + left else 0

type Grid = [[Int]]

isLand :: Int -> Bool
isLand n = n == 1
