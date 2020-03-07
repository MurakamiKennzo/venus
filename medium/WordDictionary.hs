-- Design a data structure that supports the following two operations:

-- void addWord(word)
-- bool search(word)
-- search(word) can search a literal word or a regular expression string containing only letters a-z or .. A . means it can represent any one letter.

module WordDictionary
  (
    WordDictionary(..)
  , addWord
  , search
  ) where

import Prelude hiding ( words )
import Control.Monad.State ( State
                           , state )
import Data.Maybe ( fromMaybe )
import Data.List ( find 
                 , findIndex )

addWord :: String -> State WordDictionary ()
addWord word = let word' = format word in state $ \wordDic -> ((), addWord' word' wordDic)

search :: String -> State WordDictionary Bool
search word = let word' = format word in state $ \wordDic -> (search' word' wordDic, wordDic)

addWord' :: [WordChar] -> WordDictionary -> WordDictionary
addWord' a Empty = Start :-> [fromList a]
addWord' a@(x:xs) (b :-> words) = (b :->) . fromMaybe (fromList a : words) $ do
  i <- findIndex (startsBy x) words
  let (prev, curr@(_ :-> currWords):next) = splitAt i words
  return $ prev <> [if null xs then (x :-> currWords) else addWord' xs curr] <> next

search' :: [WordChar] -> WordDictionary -> Bool
search' [] _ = True
search' _ Empty = False
search' a@(x:xs) (b :-> words) = fromMaybe False $ do
  curr@(c :-> _) <- find (startsBy x) words
  return $ if null xs then case c of
                            End _ -> True
                            _ -> False
                      else search' xs curr

fromList :: [WordChar] -> WordDictionary
fromList [] = Empty
fromList (x:xs) = x :-> [fromList xs]

startsBy :: WordChar -> WordDictionary -> Bool
startsBy _ Empty = False
startsBy Start (Start :-> _) = True
startsBy _ (Start :-> _) = False
startsBy a (b :-> _) = let m = extract a
                           n = extract b
                       in  if m == '.' then True else m == n
  where extract :: WordChar -> Char
        extract (Ongoing d) = d
        extract (End d) = d

format :: String -> [WordChar]
format [] = []
format a = (map Ongoing $ init a) <> [End $ last a]

infixr 6 :->
data WordDictionary = Empty
                    | WordChar :-> [WordDictionary] deriving (Show)

data WordChar = Start
              | Ongoing Char
              | End Char deriving (Show)
