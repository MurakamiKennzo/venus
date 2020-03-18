-- Given a complete binary tree, count the number of nodes.

-- Note:

-- Definition of a complete binary tree from Wikipedia:
-- In a complete binary tree every level, except possibly the last, is completely filled, and all nodes in the last level are as far left as possible. It can have between 1 and 2h nodes inclusive at the last level h.

module CountNodes
  (
    countNodes
  , Tree(..)
  ) where

countNodes :: Tree a -> Int
countNodes Empty = 0
countNodes (Node _ left right) = 1 + countNodes left + countNodes right

data Tree a = Empty
            | Node a (Tree a) (Tree a) deriving (Show)
