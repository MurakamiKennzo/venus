-- Serialization is the process of converting a data structure or object into a sequence of bits so that it can be stored in a file or memory buffer, or transmitted across a network connection link to be reconstructed later in the same or another computer environment.

-- Design an algorithm to serialize and deserialize a binary tree. There is no restriction on how your serialization/deserialization algorithm should work. You just need to ensure that a binary tree can be serialized to a string and this string can be deserialized to the original tree structure.

module Tree
  (
    serialize
  , deserialize
  , Tree(..)
  ) where

import Text.Read.Lex ( Lexeme( Ident ) )

serialize :: (Show a) => Tree a -> String
serialize = show . mconcat . serialize'

serialize' :: Tree a -> [[Node a]]
serialize' Empty = [[Nil]]
serialize' (Node a l r) = let b = serialize' l
                              c = serialize' r
                          in  [TNode a]:combine b c
  where combine :: [[Node a]] -> [[Node a]] -> [[Node a]]
        combine [] a = a
        combine a [] = a
        combine (x:xs) (y:ys) = x <> y :combine xs ys

deserialize :: (Read a, Eq a) => String -> Tree a
deserialize = head . foldr step [] . deconstruct 1 . read
  where step :: [Node a] -> [Tree a] -> [Tree a]
        step xs [] = map singleton xs
        step (Nil:xs) ys = Empty : step xs ys
        step (x:xs) ys = let ([a, b], c) = splitAt 2 ys
                         in  buildTree x a b :step xs c

        singleton :: Node a -> Tree a
        singleton Nil = Empty
        singleton (TNode a) = Node a Empty Empty

        buildTree :: Node a -> Tree a -> Tree a -> Tree a
        buildTree (TNode a) b c = Node a b c
        buildTree _ _ _ = Empty

deconstruct :: (Eq a) => Int -> [Node a] -> [[Node a]]
deconstruct _ [] = []
deconstruct n xs = let (a, b) = splitAt n xs
                   in  a: deconstruct ((* 2) . length . filter (/= Nil) $ a) b

data Node a = Nil
            | TNode a deriving (Eq, Ord)

instance (Show a) => Show (Node a) where
  show Nil = "null"
  show (TNode a) = show a

instance (Read a) => Read (Node a) where
  readsPrec _ s = mconcat $ do
    (a, b) <- lex s
    return $ case a of
      "null" -> [(Nil, b)]
      _ -> map (\(a, b) -> (TNode a, b)) $ reads s

data Tree a = Empty
            | Node a (Tree a) (Tree a) deriving (Show, Eq, Ord, Read)
