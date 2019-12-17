-- Given a non-empty string s and a dictionary wordDict containing a list of non-empty words, add spaces in s to construct a sentence where each word is a valid dictionary word. Return all such possible sentences.

-- Note:

-- The same word in the dictionary may be reused multiple times in the segmentation.
-- You may assume the dictionary does not contain duplicate words.

module WordBreak
  (
    wordBreak
  ) where

wordBreak :: String -> [String] -> [[String]]
wordBreak a b = filter (all (`elem` b)) . breakWord $ a

breakWord :: String -> [[String]]
breakWord "" = []
breakWord [x] = [[[x]]]
breakWord (x:xs) = let a = breakWord xs in map ([x]:) a <> map (headInsert x) a
  where headInsert :: Char -> [String] -> [String]
        headInsert c [] = [[c]]
        headInsert c (s:ss) = (c:s):ss
