-- According to the Wikipedia's article: "The Game of Life, also known simply as Life, is a cellular automaton devised by the British mathematician John Horton Conway in 1970."

-- Given a board with m by n cells, each cell has an initial state live (1) or dead (0). Each cell interacts with its eight neighbors (horizontal, vertical, diagonal) using the following four rules (taken from the above Wikipedia article):

-- Any live cell with fewer than two live neighbors dies, as if caused by under-population.
-- Any live cell with two or three live neighbors lives on to the next generation.
-- Any live cell with more than three live neighbors dies, as if by over-population..
-- Any dead cell with exactly three live neighbors becomes a live cell, as if by reproduction.
-- Write a function to compute the next state (after one update) of the board given its current state. The next state is created by applying the above rules simultaneously to every cell in the current state, where births and deaths occur simultaneously.

module GameOfLife
  (
    gameOfLife
  , fromList
  ) where

import Data.Maybe ( catMaybes )

gameOfLife :: Board Int -> Board Int
gameOfLife a@(Board r c b) = fromList . map (map (flip updateCeil a)) $ indices (r - 1) (c - 1)
  where indices :: Int -> Int -> [[(Int, Int)]]
        indices r c = foldr step [] [0 .. r]
          where step :: Int -> [[(Int, Int)]] -> [[(Int, Int)]]
                step n a = map (\x -> (n, x)) [0 .. c]:a

updateCeil :: (Int, Int) -> Board Int -> Int
updateCeil (r, c) all@(Board r' c' b)
  | d == 1 = if f == 2 || f == 3 then 1 else 0
  | d == 0 = if f == 3 then 1 else 0
  where a = catMaybes $ getArounds (r, c) all
        d = b !! r !! c
        f = length . filter (== 1) $ a

getArounds :: (Int, Int) -> Board Int -> [Maybe Int]   
getArounds (r, c) b = let a = [ (r - 1, c - 1)
                              , (r - 1, c)
                              , (r - 1, c + 1)
                              , (r, c - 1)
                              , (r, c + 1)
                              , (r + 1, c - 1)
                              , (r + 1, c)
                              , (r + 1, c + 1) ]
                     in  map (flip getAround b) a
  where getAround :: (Int, Int) -> Board Int -> Maybe Int
        getAround (r, c) (Board r' c' b)
          | r < 0 || r >= r' || c < 0 || c >= c' = Nothing
          | otherwise = return $ b !! r !! c

fromList :: [[a]] -> Board a
fromList [] = Board 0 0 []
fromList a@(x:xs) = Board (length a) (length x) a

data Board a = Board Int Int [[a]] deriving (Eq)

instance (Show a) => Show (Board a) where
  show (Board _ _ a) = show a
