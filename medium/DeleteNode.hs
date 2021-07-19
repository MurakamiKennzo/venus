-- Given a root node reference of a BST and a key, delete the node with the given key in the BST. Return the root node reference (possibly updated) of the BST.

-- Basically, the deletion can be divided into two stages:

-- Search for a node to remove.
-- If the node is found, delete the node.

module DeleteNode
  (
    deleteNode
  ) where

deleteNode :: (Ord a) => Tree a -> a -> Tree a
deleteNode Empty _ = Empty
deleteNode (Node a left right) a'
  | a /= a' = Node a (deleteNode left a') (deleteNode right a')
  | otherwise = buildNodeTree left right

buildNodeTree :: Tree a -> Tree a -> Tree a
buildNodeTree Empty right = right
buildNodeTree left Empty = left
buildNodeTree left right = let (a, right') = mostLeftLeaf right
                           in Node a left right'
  where mostLeftLeaf :: Tree a -> (a, Tree a)
        mostLeftLeaf (Node a Empty right) = (a, right)
        mostLeftLeaf (Node a left right) = let (a', b) = mostLeftLeaf left
                                           in (a', Node a b right)

data Tree a = Empty
            | Node a (Tree a) (Tree a) deriving (Show)
