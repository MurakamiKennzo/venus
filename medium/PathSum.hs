-- Given a binary tree and a sum, find all root-to-leaf paths where each path's sum equals the given sum.

-- Note: A leaf is a node with no children.

module PathSum
  (
    pathSum
  , Tree(..)
  ) where

pathSum :: (Num a, Ord a) => Tree a -> a -> [[a]]
pathSum a b = case pathSum' a b of
                Nothing -> []
                Just a -> a

pathSum' :: (Num a, Ord a) => Tree a -> a -> Maybe [[a]]
pathSum' Empty _ = Nothing
pathSum' (Node a Empty Empty) b = if a == b then Just [[a]] else Nothing
pathSum' (Node a b c) d
  | e < 0 = Nothing
  | otherwise = let f = pathSum' b e
                    g = pathSum' c e
                in  fmap (map (a:)) f +++ fmap (map (a:)) g
  where e = d - a

infix 3 +++
(+++) :: Maybe [[a]] -> Maybe [[a]] -> Maybe [[a]]
Nothing +++ a = a
a +++ Nothing = a
(Just a) +++ (Just b) = Just (a ++ b)

data Tree a = Empty
            | Node a (Tree a) (Tree a) deriving (Eq, Show)
