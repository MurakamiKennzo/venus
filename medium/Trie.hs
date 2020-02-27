-- mplement a trie with insert, search, and startsWith methods.

module Trie
  (
    Trie(..)
  , insert
  , search
  , startsWith
  ) where

import Control.Monad.State ( state
                           , State )
import Data.List ( find
                 , findIndex )

insert :: (Eq a) => [a] -> State (Trie a) ()
insert a = let b = toTrieNodeList a in state $ \trie -> ((), insert' b trie)

search :: (Eq a) => [a] -> State (Trie a) Bool
search a = let b = toTrieNodeList a in state $ \trie -> (search' b trie, trie)

startsWith :: (Eq a) => [a] -> State (Trie a) Bool
startsWith a = let b = toTrieNodeList a in state $ \trie -> (startsWith' b trie, trie)

insert' :: (Eq a) => [TrieNode a] -> Trie a -> Trie a
insert' [] trie = trie
insert' a Empty = Start :-> [formList a]
insert' a@(x:xs) (node :-> tries) = (node :->) . maybe (formList a:tries) id $ do
  i <- findIndex (startTrieNode x) tries
  let (ftries, ctrie: btries) = splitAt i tries
      ctrie' = if null xs
                then case ctrie of
                      (OnGoing m :-> n) -> End m :-> n
                      _ -> ctrie
                else insert' xs ctrie
  return $ ftries <> [ctrie'] <> btries

search' :: (Eq a) => [TrieNode a] -> Trie a -> Bool
search' _ Empty = False
search' [] _ = True
search' a@(x:xs) (node :-> tries) = maybe False id $ do
  trie <- find (startTrieNode x) tries
  return $ if null xs
            then case trie of
                    (End _ :-> _) -> True
                    _ -> False
            else search' xs trie

startsWith' :: (Eq a) => [TrieNode a] -> Trie a -> Bool
startsWith' _ Empty = False
startsWith' [] _ = True
startsWith' a@(x:xs) (node :-> tries) = maybe False id $ do
  trie <- find (startTrieNode x) tries
  return $ startsWith' xs trie

toTrieNodeList :: (Eq a) => [a] -> [TrieNode a]
toTrieNodeList [] = []
toTrieNodeList a = let b = map OnGoing $ init a
                       c = End $ last a
                   in  b <> [c]

formList :: [TrieNode a] -> Trie a
formList [] = Empty
formList (x:xs) = x :-> [formList xs]

startTrieNode :: (Eq a) => TrieNode a -> Trie a -> Bool
startTrieNode _ Empty = False
startTrieNode Start (Start :-> _) = True
startTrieNode Start _ = False
startTrieNode a (b :-> _) = extract a == extract b
  where extract :: TrieNode a -> a
        extract (OnGoing x) = x
        extract (End x) = x

data Trie a = Empty
            | TrieNode a :-> [Trie a] deriving (Show, Eq)

data TrieNode a = Start
                | OnGoing a
                | End a deriving (Show, Eq)
