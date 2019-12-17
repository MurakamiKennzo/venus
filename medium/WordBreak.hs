-- Given a non-empty string s and a dictionary wordDict containing a list of non-empty words, determine if s can be segmented into a space-separated sequence of one or more dictionary words.

-- Note:

-- The same word in the dictionary may be reused multiple times in the segmentation.
-- You may assume the dictionary does not contain duplicate words.

module WordBreak
  (
    wordBreak
  ) where

import Data.List ( isPrefixOf )

wordBreak :: String -> [String] -> Bool
wordBreak "" _ = True
wordBreak a@(x:_) b = any (flip wordBreak b) . map (flip drop a) . map length . filter (flip isPrefixOf a) . filter (isPrefixOf [x]) $ b
