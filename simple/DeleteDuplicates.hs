-- Given a sorted linked list, delete all duplicates such that each element appear only once.

module DeleteDuplicates
  (
    deleteDuplicates
  , fromList  
  ) where

deleteDuplicates :: (Eq a) => Link a -> Link a
deleteDuplicates Empty = Empty
deleteDuplicates a'@(a :-> Empty) = a'
deleteDuplicates (a :-> b'@(b :-> _)) = let c = deleteDuplicates b' in if a == b then c else a :-> c

data Link a = Empty | a :-> Link a deriving (Eq, Show)

infixr 4 :->

fromList :: [a] -> Link a
fromList = foldr (:->) Empty
