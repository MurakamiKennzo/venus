-- Given a singly linked list, determine if it is a palindrome.

module IsPalindrome3
  (
    isPalindrome
  , Link(..)
  ) where

isPalindrome :: (Eq a) => Link a -> Bool
isPalindrome = do
  x <- id
  y <- reverse'
  return $ x == y

reverse' :: Link a -> Link a
reverse' Empty = Empty
reverse' (a :-> link) = reverse' link <> (a :-> Empty)

instance Semigroup (Link a) where
  Empty <> a = a
  a <> Empty = a
  (a :-> link) <> b = a :-> (link <> b)

infixr 6 :->
data Link a = Empty
            | a :-> Link a deriving (Eq, Show)
