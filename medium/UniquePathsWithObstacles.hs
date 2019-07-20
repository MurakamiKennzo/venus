-- A robot is located at the top-left corner of a m x n grid (marked 'Start' in the diagram below).

-- The robot can only move either down or right at any point in time. The robot is trying to reach the bottom-right corner of the grid (marked 'Finish' in the diagram below).

-- Now consider if some obstacles are added to the grids. How many unique paths would there be?

-- An obstacle and empty space is marked as 1 and 0 respectively in the grid.

-- Note: m and n will be at most 100.

module UniquePathsWithObstacles
  (
    uniquePathsWithObstacles
  ) where

uniquePathsWithObstacles :: [[Int]] -> Int
uniquePathsWithObstacles [[_]] = 1
uniquePathsWithObstacles [(x:xs)] = if x /= 1 then uniquePathsWithObstacles [xs] else 0
uniquePathsWithObstacles ([x]:xs) = if x /= 1 then uniquePathsWithObstacles xs else 0
uniquePathsWithObstacles a@(x:xs) = if head x /= 1 
                                      then uniquePathsWithObstacles xs + (uniquePathsWithObstacles . map (drop 1) $ a)
                                      else 0
