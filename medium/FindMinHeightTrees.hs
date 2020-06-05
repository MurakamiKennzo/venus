-- For an undirected graph with tree characteristics, we can choose any node as the root. The result graph is then a rooted tree. Among all possible rooted trees, those with minimum height are called minimum height trees (MHTs). Given such a graph, write a function to find all the MHTs and return a list of their root labels.

-- Format
-- The graph contains n nodes which are labeled from 0 to n - 1. You will be given the number n and a list of undirected edges (each edge is a pair of labels).

-- You can assume that no duplicate edges will appear in edges. Since all edges are undirected, [0, 1] is the same as [1, 0] and thus will not appear together in edges.

module FindMinHeightTrees
  (
    findMinHeightTrees
  ) where

import Data.List ( partition
                 , sortOn
                 , groupBy )
import Data.Function ( on )

findMinHeightTrees :: Int -> [Edge] -> [Int]
findMinHeightTrees 0 _ = []
findMinHeightTrees n xs = map treeRoot . map snd . head . groupBy ((==) `on` fst) . sortOn fst . map assocHeight $ [ buildTree x xs | x <- [0 .. pred n] ]
  where buildTree :: Int -> [Edge] -> Tree Int
        buildTree n xs = let (a, b) = partition (n `elemIn`) xs
                             c = map (another n) a
                         in  n :-> map (flip buildTree b) c

        elemIn :: (Eq a) => a -> (a, a) -> Bool
        elemIn a (b, c)
          | a == b || a == c = True
          | otherwise = False
        
        another :: (Eq a) => a -> (a, a) -> a
        another a (b, c)
          | a == b = c
          | a == c = b

        height :: Tree a -> Int
        height Empty = 0
        height (_ :-> []) = 1
        height (_ :-> xs) = 1 + maximum (map height xs)

        assocHeight :: Tree a -> (Int, Tree a)
        assocHeight tree = (height tree, tree)

        treeRoot :: Tree a -> a
        treeRoot (a :-> _) = a

type Edge = (Int, Int)

infixr 6 :->
data Tree a = Empty
            | a :-> [Tree a] deriving (Show, Eq, Ord)
