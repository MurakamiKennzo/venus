-- Serialization is converting a data structure or object into a sequence of bits so that it can be stored in a file or memory buffer, or transmitted across a network connection link to be reconstructed later in the same or another computer environment.

-- Design an algorithm to serialize and deserialize a binary search tree. There is no restriction on how your serialization/deserialization algorithm should work. You need to ensure that a binary search tree can be serialized to a string, and this string can be deserialized to the original tree structure.

-- The encoded string should be as compact as possible.

module BinarySearchTreeSerializeAndDeserialize
  (
    serialize
  , deserialize
  ) where

serialize :: (Ord a, Show a, Read a) => Tree a -> String
serialize = show . serialize'
  where serialize' :: (Ord a, Show a, Read a) => Tree a -> [a]
        serialize' Empty = []
        serialize' (a :-> (l, r)) = a: (serialize' l) <> (serialize' r)

deserialize :: (Ord a, Show a, Read a) => String -> Tree a
deserialize = deserialize' . read
  where deserialize' :: (Ord a, Show a, Read a) => [a] -> Tree a
        deserialize' [] = Empty
        deserialize' (x:xs) = let (l, r) = span (< x) xs
                              in  x :-> (deserialize' l, deserialize' r)

infixr 4 :->
data Tree a = Empty
            | a :-> (Tree a, Tree a) deriving (Show)
