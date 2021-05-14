-- Given an m x n matrix board where each cell is a battleship 'X' or empty '.', return the number of the battleships on board.

-- Battleships can only be placed horizontally or vertically on board. In other words, they can only be made of the shape 1 x k (1 row, k columns) or k x 1 (k rows, 1 column), where k can be of any size. At least one horizontal or vertical cell separates between two battleships (i.e., there are no adjacent battleships).

module CountBattleships
  (
    countBattleships
  ) where

import Control.Monad ( guard )

countBattleships :: [[Char]] -> Int
countBattleships xs = sum $ do 
  i <- [0 .. length xs - 1]
  j <- [0 .. length (xs !! i) - 1]
  guard $ isBattleships (i, j) xs
  return 1
  where isBattleships :: (Int, Int) -> [[Char]] -> Bool
        isBattleships (i, j) xs = xs !! i !! j == 'X' 
                                    && ((i - 1) < 0 || xs !! (i - 1) !! j == '.') 
                                    && ((j - 1) < 0 || xs !! i !! (j - 1) == '.')
