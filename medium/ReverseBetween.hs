-- Reverse a linked list from position m to n.

-- Note: 1 ≤ m ≤ n ≤ length of list.

module ReverseBetween
  (
    reverseBetween
  , fromList
  ) where

import Prelude hiding ( drop
                      , take
                      , reverse )

reverseBetween :: Link a -> Int -> Int ->Link a
reverseBetween link a b = let a' = a - 1
                              c = take a' link
                              d = drop a' link
                              e = take (b - a') d
                              f = drop b link
                          in  c <> reverse e <> f

data Link a = Empty | a :-> Link a deriving (Show)
  
infixr 4 :->

fromList :: [a] -> Link a
fromList = foldr (:->) Empty

instance Foldable Link where
  foldMap f Empty = mempty
  foldMap f (a :-> link) = f a <> foldMap f link 

instance Semigroup (Link a) where
  Empty <> a = a
  (a :-> link) <> b = a :-> link <> b

instance Monoid (Link a) where
  mempty = Empty

take :: Int -> Link a -> Link a
take 0 _ = Empty
take _ Empty = Empty
take n (a :-> link) = a :-> take (n - 1) link

drop :: Int -> Link a -> Link a
drop 0 a = a
drop _ Empty = Empty
drop n (_ :-> link) = drop (n - 1) link

reverse :: Link a -> Link a
reverse = reverse' Empty
  where reverse' :: Link a -> Link a -> Link a
        reverse' a Empty = a
        reverse' b (a :-> link) = reverse' (a :-> b) link
