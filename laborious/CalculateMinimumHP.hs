-- Some of the rooms are guarded by demons, so the knight loses health (negative integers) upon entering these rooms; other rooms are either empty (0's) or contain magic orbs that increase the knight's health (positive integers).

-- In order to reach the princess as quickly as possible, the knight decides to move only rightward or downward in each step.

-- Write a function to determine the knight's minimum initial health so that he is able to rescue the princess.

module CalculateMinimumHP
  (
    calculateMinimumHP
  ) where

calculateMinimumHP :: Dungeon -> Int
calculateMinimumHP = life . calculateMinimumHP'
  where life :: Int -> Int
        life x = if x >= 0 then 1 else 1 + abs x

calculateMinimumHP' :: Dungeon -> Int
calculateMinimumHP' [] = 0
calculateMinimumHP' [a] = sum a
calculateMinimumHP' a@(x:xs)
  | length x == 1 = sum . map head $ a
  | otherwise = let c = x !! 0
                    downDungeon = xs
                    rightDungeon = map tail a
                in  c + max (calculateMinimumHP' downDungeon) (calculateMinimumHP' rightDungeon)

type Dungeon = [[Int]]
