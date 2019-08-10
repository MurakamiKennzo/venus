-- Given a sorted linked list, delete all nodes that have duplicate numbers, leaving only distinct numbers from the original list.

module DeleteDuplicates
  (
    deleteDuplicates
  , fromList  
  ) where

deleteDuplicates :: (Eq a) => Link a -> Link a
deleteDuplicates Empty = Empty
deleteDuplicates a'@(a :-> Empty) = a'
deleteDuplicates a'@(a :-> b :-> Empty) = if a == b then Empty else a'
deleteDuplicates (a :-> b'@(b :-> c'@(c :-> _))) = if a == b 
                                              then deleteDuplicates $ if b == c then b' else c'
                                              else a :-> deleteDuplicates b'

data Link a = Empty | a :-> Link a deriving (Eq, Show)

infixr 4 :->

fromList :: [a] -> Link a
fromList = foldr (:->) Empty
