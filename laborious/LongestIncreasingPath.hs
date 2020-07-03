-- Given an integer matrix, find the length of the longest increasing path.

-- From each cell, you can either move to four directions: left, right, up or down. You may NOT move diagonally or move outside of the boundary (i.e. wrap-around is not allowed).

module LongestIncreasingPath
  (
    longestIncreasingPath
  ) where

import Data.Maybe ( fromJust )
import Data.List ( nub )

longestIncreasingPath :: (Ord a) => [[a]] -> Int
longestIncreasingPath xs = let a@(Matrix (r, c) _) = fromList xs
                           in  maximum . map length . concat $ [ roads (r', c') a | r' <- [0 .. r], c' <- [0 .. c] ]

fromList :: [[a]] -> Matrix a
fromList [] = Matrix (0, 0) []
fromList a@(x:xs) = Matrix (length a - 1, length x - 1) a

roads :: (Ord a) => (Int, Int) -> Matrix a -> [[a]]
roads (r, c) m@(Matrix (r', c') xs)
  | r < 0 || r > r' || c < 0 || c > c' = []
  | otherwise = let bbs = if bb > aa then map (aa':) (roads (r - 1, c) m) else [[aa']]
                    ccs = if cc > aa then map (aa':) (roads (r, c + 1) m) else [[aa']]
                    dds = if dd > aa then map (aa':) (roads (r + 1, c) m) else [[aa']]
                    ees = if ee > aa then map (aa':) (roads (r, c - 1) m) else [[aa']]
                in  nub $ bbs <> ccs <> dds <> ees
  where aa = xs !!! r >>= (!!! c)
        aa' = fromJust aa
        bb = xs !!! (r - 1) >>= (!!! c)
        cc = xs !!! r >>= (!!! (c + 1))
        dd = xs !!! (r + 1) >>= (!!! c)
        ee = xs !!! r >>= (!!! (c - 1))

(!!!) :: [a] -> Int -> Maybe a
xs !!! n
  | n < 0 || n >= length xs = Nothing
  | otherwise = return $ xs !! n

data Matrix a = Matrix (Int, Int) [[a]] deriving (Show, Eq, Ord)
