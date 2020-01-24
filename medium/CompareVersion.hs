-- Compare two version numbers version1 and version2.
-- If version1 > version2 return 1; if version1 < version2 return -1;otherwise return 0.

-- You may assume that the version strings are non-empty and contain only digits and the . character.

-- The . character does not represent a decimal point and is used to separate number sequences.

-- For instance, 2.5 is not "two and a half" or "half way to version three", it is the fifth second-level revision of the second first-level revision.

-- You may assume the default revision number for each level of a version number to be 0. For example, version number 3.4 has a revision number of 3 and 4 for its first and second level revision number. Its third and fourth level revision number are both 0.

-- Ordering => LT :: -1
--             EQ :: 0
--             GT :: 1

module CompareVersion
  (
    compareVersion
  ) where

import Data.List (dropWhileEnd)

compareVersion :: Version -> Version -> Ordering
compareVersion a b = let aa = dropWhileEnd (== 0) . map read . split '.' $ a :: [Int]
                         bb = dropWhileEnd (== 0). map read . split '.' $ b :: [Int]
                     in  compare aa bb

type Version = String

split :: Char -> String -> [String]
split _ [] = []
split c xs = let (a, b) = break (== c) xs
             in  a : split c (if null b then b else tail b)
