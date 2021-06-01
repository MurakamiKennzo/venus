-- Given a n * n matrix grid of 0's and 1's only. We want to represent the grid with a Quad-Tree.

-- Return the root of the Quad-Tree representing the grid.

-- Notice that you can assign the value of a node to True or False when isLeaf is False, and both are accepted in the answer.

-- A Quad-Tree is a tree data structure in which each internal node has exactly four children. Besides, each node has two attributes:

--   val: True if the node represents a grid of 1's or False if the node represents a grid of 0's. 
--   isLeaf: True if the node is leaf node on the tree or False if the node has the four children.

-- class Node {
--     public boolean val;
--     public boolean isLeaf;
--     public Node topLeft;
--     public Node topRight;
--     public Node bottomLeft;
--     public Node bottomRight;
-- }
-- We can construct a Quad-Tree from a two-dimensional area using the following steps:

--   If the current grid has the same value (i.e all 1's or all 0's) set isLeaf True and set val to the value of the grid and set the four children to Null and stop.
--   If the current grid has different values, set isLeaf to False and set val to any value and divide the current grid into four sub-grids as shown in the photo.
--   Recurse for each of the children with the proper sub-grid.

module Construct
  (
    construct
  ) where

import Control.Monad ( mzero )

construct :: Grid Int -> Maybe (QuadTree Bool)
construct [] = mzero
construct ([]:_) = mzero
construct [[x]] = return $ QuadTree (x == 1) True Nothing Nothing Nothing Nothing
construct xs@((x:_):_)
  | b = return $ QuadTree (x == 1) True Nothing Nothing Nothing Nothing
  | otherwise = let n' = n `div` 2
                    topLeft = construct $ subGrid (0, 0) (n', n') xs
                    topRight = construct $ subGrid (n', 0) (n, n) xs
                    bottomLeft = construct $ subGrid (0, n') (n', n') xs
                    bottomRight = construct $ subGrid (n', n') (n, n) xs
                in  return $ QuadTree True False topLeft topRight bottomLeft bottomRight
  where n = length xs
        b = all (== x) . foldMap id $ xs

subGrid :: (Int, Int) -> (Int, Int) -> Grid a -> Grid a
subGrid (0, 0) (x, _) = map (take x) . take x
subGrid (x, 0) _ = map (drop x) . take x
subGrid (0, x) _ = map (take x) . drop x
subGrid (x, _) _ = map (drop x) . drop x

data QuadTree a = QuadTree { val :: a
                           , isLeaf :: Bool
                           , topLeft :: Maybe (QuadTree a)
                           , topRight :: Maybe (QuadTree a)
                           , bottomLeft :: Maybe (QuadTree a)
                           , bottomRight :: Maybe (QuadTree a) } deriving (Show)

type Grid a = [[a]]
