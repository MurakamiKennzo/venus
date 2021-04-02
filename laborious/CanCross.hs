-- A frog is crossing a river. The river is divided into some number of units, and at each unit, there may or may not exist a stone. The frog can jump on a stone, but it must not jump into the water.

-- Given a list of stones' positions (in units) in sorted ascending order, determine if the frog can cross the river by landing on the last stone. Initially, the frog is on the first stone and assumes the first jump must be 1 unit.

-- If the frog's last jump was k units, its next jump must be either k - 1, k, or k + 1 units. The frog can only jump in the forward direction.

module CanCross
  (
    canCross
  ) where

canCross :: Stones -> Bool
canCross = canCross' 0

canCross' :: Int -> Stones -> Bool
canCross' _ [] = True
canCross' n [_] = if n < 0 then False else True
canCross' n (x:xs) = or [ canCross'' (n - 1) (x + n - 1) xs
                        , canCross'' n (x + n) xs
                        , canCross'' (n + 1) (x + n + 1) xs ]

canCross'' :: Int -> Int -> Stones -> Bool
canCross'' n x xs
  | null xs = False
  | otherwise = or [ canCross' n xs'' | xs'' <- xs' ]
  where xs' = recDropWhile x xs

recDropWhile :: Int -> [Int] -> [[Int]]
recDropWhile x xs
  | null xs' = []
  | otherwise = xs' : recDropWhile x (tail xs')
  where xs' = dropWhile (/= x) xs
  
type Stones = [Int]
