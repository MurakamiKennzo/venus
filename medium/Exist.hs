-- Given a 2D board and a word, find if the word exists in the grid.

module Exist
  (
    exist
  ) where

import Data.List (delete)

exist :: [[Char]] -> String -> Bool
exist a b = let a' = buildRoads a
                b' = buildRoads [b]
            in  b' -: a'

infixl 1 -:
(-:) :: Roads -> Roads -> Bool
[] -: _ = True
(x:xs) -: a = (x `elem` a) && (xs -: delete x a)

type Roads = [Road]

data Road = Road Char Char deriving (Show)

buildRoads :: [[Char]] -> Roads
buildRoads [] = []
buildRoads [x] = rowRoad x
buildRoads (x:y:xs) = rowRoad x ++ colRoad x y ++ buildRoads (y:xs)

rowRoad :: [Char] -> Roads
rowRoad [] = []
rowRoad [_] = []
rowRoad (x:y:xs) = Road x y : rowRoad (y:xs)

colRoad :: [Char] -> [Char] -> Roads
colRoad [] [] = []
colRoad (x:xs) (y:ys) = Road x y : colRoad xs ys


instance Eq Road where
  (Road a b) == (Road x y) = let c = a == x && b == y
                                 d = a == y && b == x
                             in  c || d
