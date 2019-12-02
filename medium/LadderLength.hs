-- Given two words (beginWord and endWord), and a dictionary's word list, find the length of shortest transformation sequence from beginWord to endWord, such that:

-- Only one letter can be changed at a time.
-- Each transformed word must exist in the word list. Note that beginWord is not a transformed word.
-- Note:

-- Return 0 if there is no such transformation sequence.
-- All words have the same length.
-- All words contain only lowercase alphabetic characters.
-- You may assume no duplicates in the word list.
-- You may assume beginWord and endWord are non-empty and are not the same.

module LadderLength
  (
    ladderLength
  ) where

import Prelude hiding (Word)
import Data.List ( sortOn
                 , delete
                 , dropWhileEnd )

ladderLength :: Word -> Word -> [Word] -> Int
ladderLength a b c = if b `elem` c then f else 0
  where d = findLadders a c
        e = map (dropWhileEnd (/= b)) d
        f = length . head . filter (not . null) . sortOn length $ e

findLadders :: Word -> [Word] -> [[Word]]
findLadders a [] = [[a]]
findLadders a b = let c = filter (single a) b
                      d = map (\x -> findLadders x (delete x b)) c
                   in  if null c then [[a]] else map (a:) $ concat d

single :: Word -> Word -> Bool
single a b = diff a b == 1
  where diff :: Word -> Word -> Int
        diff _ [] = 0
        diff [] _ = 0
        diff (x:xs) (y:ys) = diff xs ys + if x == y then 0 else 1

type Word = String
