-- Determine if a 9x9 Sudoku board is valid. Only the filled cells need to be validated according to the following rules:

-- Each row must contain the digits 1-9 without repetition.
-- Each column must contain the digits 1-9 without repetition.
-- Each of the 9 3x3 sub-boxes of the grid must contain the digits 1-9 without repetition.

module IsValidSudoku
  (
    isValidSudoku
  ) where

import Data.List (nub)
import Data.Function (on)
import Control.Applicative

isValidSudoku :: Sudoku -> Bool
isValidSudoku a = and . getZipList $ ZipList [isValidRow, isValidColumn, isValidBoxed] <*> ZipList (repeat a)

type Sudoku = [[Char]]

isValidRow :: Sudoku -> Bool
isValidRow = and . map uniq

isValidColumn :: Sudoku -> Bool
isValidColumn a = and . map uniq $ map (\i -> map (!! i) a) [0 .. 8]

isValidBoxed :: Sudoku -> Bool
isValidBoxed a = 
  let indices = [[(x, y) | x <- xs, y <- ys]
                         | let indices = [[0 .. 2], [3 .. 5], [6 .. 8]], xs <- indices, ys <- indices]
  in  and . map uniq . map (map (\(x, y) -> a !! x !! y)) $ indices

uniq :: [Char] -> Bool
uniq a = ((==) `on` length) (nub b) b
  where b = filter (/= '.') a
