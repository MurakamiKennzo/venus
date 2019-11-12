-- Given a non-empty binary tree, find the maximum path sum.

-- For this problem, a path is defined as any sequence of nodes from some starting node to any node in the tree along the parent-child connections. The path must contain at least one node and does not need to go through the root.

module MaxPathSum 
  (
    maxPathSum
  , Tree(..)
  ) where

maxPathSum :: (Num a, Ord a) => Tree a -> Maybe a
maxPathSum Empty = Nothing
maxPathSum (Node a Empty Empty) = Just a
maxPathSum (Node a b c) = let d = Just a
                              e = maxPathSum b
                              f = maxPathSum c
                              g = maxPathSum' b
                              h = maxPathSum' c
                          in  maximum [ d
                                      , e
                                      , f
                                      , (+) <$> d <*> g
                                      , (+) <$> d <*> h
                                      , (+) <$> ((+) <$> d <*> g) <*> h ]

maxPathSum' :: (Num a, Ord a) => Tree a -> Maybe a
maxPathSum' Empty = Nothing
maxPathSum' (Node a Empty Empty) = Just a
maxPathSum' (Node a b c) = let d = Just a
                               e = maxPathSum' b
                               f = maxPathSum' c
                           in  maximum [ d
                                       , e 
                                       , f
                                       , (+) <$> d <*> e 
                                       , (+) <$> d <*> f ]

data Tree a = Empty
            | Node a (Tree a) (Tree a) deriving (Show, Eq)
