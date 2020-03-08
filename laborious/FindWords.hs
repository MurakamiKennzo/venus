-- Given a 2D board and a list of words from the dictionary, find all words in the board.

-- Each word must be constructed from letters of sequentially adjacent cell, where "adjacent" cells are those horizontally or vertically neighboring. The same letter cell may not be used more than once in a word.

module FindWords
  (
    findWords
  ) where

import Data.List ( findIndices
                 , elemIndices )

findWords :: Words -> Board -> Words
findWords w b = filter (`inBoard` b) w

inBoard :: String -> Board -> Bool
inBoard "" _ = True
inBoard a@(x:xs) board = any (inBoardAt a board) $ findIndices' x board

findIndices' :: Char -> Board -> [(Int, Int)]
findIndices' c b = let indices = findIndices (c `elem`) b
                       jndices = map (elemIndices c) . map (b !!) $ indices
                   in  zip' indices jndices

zip' :: [a] -> [[a]] -> [(a, a)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = map (\m -> (x, m)) y <> zip' xs ys

inBoardAt :: String -> Board -> (Int, Int) -> Bool
inBoardAt "" _ _ = True
inBoardAt _ [] _ = False
inBoardAt (x:xs) board@(f:_) (i, j)
  | i < 0 = False
  | j < 0 = False
  | i >= length board = False
  | j >= length f = False
  | otherwise = let y = board !! i !! j
                    a = inBoardAt xs board
                in  if x == y
                      then or [ a (pred i, j)
                              , a (succ i, j)
                              , a (i, pred j)
                              , a (i, succ j) ]
                      else False

type Words = [String]

type Board = [String]
