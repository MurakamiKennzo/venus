-- Given a singly linked list, group all odd nodes together followed by the even nodes. Please note here we are talking about the node number and not the value in the nodes.

module OddEvenList
  (
    oddEvenList
  , Link(..)
  ) where

oddEvenList :: Link a -> Link a
oddEvenList = do
  x <- odd'
  y <- even'
  return $ x <> y

odd' :: Link a -> Link a
odd' Empty = Empty
odd' a@(_ :-> Empty) = a
odd' (x :-> y :-> xs) = x :-> odd' xs

even' :: Link a -> Link a
even' Empty = Empty
even' (_ :-> xs) = odd' xs

instance Semigroup (Link a) where
  Empty <> a = a
  a <> Empty = a
  (x :-> a) <> b = x :-> a <> b

instance Monoid (Link a) where
  mempty = Empty

infixr 5 :->
data Link a = Empty
            | a :-> Link a deriving (Show, Eq, Ord)
