-- Given a string S and a string T, find the minimum window in S which will contain all the characters in T in complexity O(n).

-- Example:

-- Input: S = "ADOBECODEBANC", T = "ABC"
-- Output: "BANC"
-- Note:

-- If there is no such window in S that covers all characters in T, return the empty string "".
-- If there is such window, you are guaranteed that there will always be only one unique minimum window in S.

module MinWindow
  (
    minWindow
  ) where

import Data.Monoid ( First(..) )
import Data.List ( delete
                 , sortOn )

minWindow :: String -> String -> String
minWindow a b = case getFirst . mconcat . fmap First . sortOn (fmap length) . fmap (flip windowString b) . splitWindow a $ b of
                  Nothing -> ""
                  Just x -> x

splitWindow :: String -> String -> [String]
splitWindow a b = let c = dropWhile (not . flip elem b) a
                  in  if c == [] then [] else c : splitWindow (tail c) b

windowString :: String -> String -> Maybe String
windowString [] (x:_) = Nothing
windowString _ [] = Just []
windowString (x:xs) b = return (x:) <*> windowString xs (delete x b)
