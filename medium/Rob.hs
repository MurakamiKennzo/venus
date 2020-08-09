-- The thief has found himself a new place for his thievery again. There is only one entrance to this area, called the "root." Besides the root, each house has one and only one parent house. After a tour, the smart thief realized that "all houses in this place forms a binary tree". It will automatically contact the police if two directly-linked houses were broken into on the same night.

-- Determine the maximum amount of money the thief can rob tonight without alerting the police.

module Rob
  (
    rob
  ) where

rob :: (Num a, Ord a) => Tree a -> a
rob Empty = 0
rob (Node a left right) = max (a + rob' left + rob' right) (rob left + rob right)

rob' :: (Num a, Ord a) => Tree a -> a
rob' Empty = 0
rob' (Node _ left right) = rob left + rob right

data Tree a = Empty
            | Node a (Tree a) (Tree a)
