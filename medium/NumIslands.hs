-- Given a 2d grid map of '1's (land) and '0's (water), count the number of islands. An island is surrounded by water and is formed by connecting adjacent lands horizontally or vertically. You may assume all four edges of the grid are all surrounded by water.

-- Example 1:

-- Input:
-- 11110
-- 11010
-- 11000
-- 00000

-- Output: 1
-- Example 2:

-- Input:
-- 11000
-- 11000
-- 00100
-- 00011

-- Output: 3

module NumIslands
  (
    numIslands
  ) where

import Data.Sequence ( Seq( (:<|) )
                     , index
                     , update
                     , findIndexL
                     , fromList )

numIslands :: [[Int]] -> Int
numIslands = numIslands' . fromList . map fromList

numIslands' :: Grid -> Int
numIslands' grid = maybe 0 (succ . numIslands' . flip changeIsland grid) $ do
  i <- findIndexL (any (== 1)) grid
  j <- findIndexL (== 1) $ grid `index` i
  return (i, j)
  
changeIsland :: Index -> Grid -> Grid
changeIsland (i, j) grid = let newGrid = update i (update j 0 $ grid `index` i) grid
                           in  moveLeft (i, j) . moveDown (i, j) . moveRight (i, j) . moveTop (i, j) $ newGrid

moveTop :: Index -> Grid -> Grid
moveTop (i, j) grid
  | i == 0 = grid
  | otherwise = let i' = pred i in if grid `index` i' `index` j == 1 then changeIsland (i', j) grid else grid

moveRight :: Index -> Grid -> Grid
moveRight (i, j) grid@(x :<| _)
  | j == length x - 1 = grid
  | otherwise = let j' = succ j in if grid `index` i `index` j' == 1 then changeIsland (i, j') grid else grid

moveDown :: Index -> Grid -> Grid
moveDown (i, j) grid
  | i == length grid - 1 = grid
  | otherwise = let i' = succ i in if grid `index` i' `index` j == 1 then changeIsland (i', j) grid else grid

moveLeft :: Index -> Grid -> Grid
moveLeft (i, j) grid@(x :<| _)
  | j == 0 = grid
  | otherwise = let j' = pred j in if grid `index` i `index` j' == 1 then changeIsland (i, j') grid else grid

type Grid = Seq (Seq Int)

type Index = (Int, Int)
