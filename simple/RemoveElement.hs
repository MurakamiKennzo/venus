-- Given an array nums and a value val, remove all instances of that value and return the new length.

module RemoveElement
  (
    removeElement
  ) where

removeElement :: (Eq a) => [a] -> a -> Int
removeElement xs a = length . deleteElement a $ xs

deleteElement :: (Eq a) => a -> [a] -> [a]
deleteElement _ [] = []
deleteElement a (x:xs)
  | a == x = b
  | otherwise = x:b
  where b = deleteElement a xs
