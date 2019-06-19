-- Write a program to solve a Sudoku puzzle by filling the empty cells.

-- A sudoku solution must satisfy all of the following rules:

-- Each of the digits 1-9 must occur exactly once in each row.
-- Each of the digits 1-9 must occur exactly once in each column.
-- Each of the the digits 1-9 must occur exactly once in each of the 9 3x3 sub-boxes of the grid.
-- Empty cells are indicated by the character '.'.

module SolveSudoku
  (
    solveSudoku
  ) where

import Data.List ( elemIndex
                 , elem
                 , findIndex
                 , find )

type Sudoku = [String]
type SudokuRows = [String]
type SudokuColumns = [String]
type SudokuBoxes = [String]
type Row = Int
type Column = Int

getSudokuRows :: Sudoku -> SudokuRows
getSudokuRows = id

getSudokuColumns :: Sudoku -> SudokuColumns
getSudokuColumns s = map (\col -> map (!! col) s) [0 .. 8]

getSudokuBoxes :: Sudoku -> SudokuBoxes
getSudokuBoxes s = [ box s rows cols | let a = [[0 .. 2], [3 .. 5], [6 .. 8]], rows <- a, cols <- a]
  where box :: Sudoku -> [Int] -> [Int] -> String
        box s' rows cols = [ s' !! x !! y | x <- rows, y <- cols ]

solveSudoku :: Sudoku -> Maybe Sudoku
solveSudoku s = case rowCol s of
                  Nothing -> Just s
                  Just (row, col) -> case placeOnes (row, col) s of
                                        [] -> Nothing
                                        xs -> let newSudokus = map (\x -> update (row, col) x s) xs
                                              in  case find (/= Nothing) . map solveSudoku $ newSudokus of
                                                    Nothing -> Nothing
                                                    Just a -> a

rowCol :: Sudoku -> Maybe (Row, Column)
rowCol s = 
  let row = elem '.' `findIndex` s
      col = case row of
              Nothing -> Nothing
              Just index -> '.' `elemIndex` (s !! index)
  in case row of
      Nothing -> Nothing
      Just i -> case col of
                  Nothing -> Nothing
                  Just j -> Just (i, j)

placeOnes :: (Row, Column) -> Sudoku -> [Char]
placeOnes (row, col) s =
  let r = getSudokuRows s !! row
      c = getSudokuColumns s !! col
      b = getSudokuBoxes s !! ((row `div` 3) * 3 + col `div` 3)
  in  [x | x <- ['1' .. '9'], x `notElem` r, x `notElem` c, x `notElem` b]

update :: (Row, Column) -> Char -> Sudoku -> Sudoku
update (row, col) a s =
  let f = take row s
      c = let item = s !! row in [take col item ++ [a] ++ drop (col + 1) item]
      l = drop (row + 1) s
  in  f ++ c ++ l
