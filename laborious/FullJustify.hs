-- Given an array of words and a width maxWidth, format the text such that each line has exactly maxWidth characters and is fully (left and right) justified.

-- You should pack your words in a greedy approach; that is, pack as many words as you can in each line. Pad extra spaces ' ' when necessary so that each line has exactly maxWidth characters.

-- Extra spaces between words should be distributed as evenly as possible. If the number of spaces on a line do not divide evenly between words, the empty slots on the left will be assigned more spaces than the slots on the right.

-- For the last line of text, it should be left justified and no extra space is inserted between words.

-- Note:

-- A word is defined as a character sequence consisting of non-space characters only.
-- Each word's length is guaranteed to be greater than 0 and not exceed maxWidth.
-- The input array words contains at least one word.

module FullJustify
  (
    fullJustify
  ) where

import Control.Applicative ( ZipList(..) )

fullJustify :: [String] -> Int -> [String]
fullJustify s a
  | wordslength <= a = [take a $ w ++ repeat ' ']
  | otherwise = let (b, c) = takeString "" a s
                    d = adjustString a b
                in  d : fullJustify c a
  where w = unwords s
        wordslength = length w

takeString :: String -> Int -> [String] -> (String, [String])
takeString c m a@(x:xs)
  | length c + length x + 1 > m = (c, a)
  | length c + length x + 1 == m = (c ++ " " ++ x, xs)
  | otherwise = takeString (if c == "" then x else c ++ " " ++ x) m xs

adjustString :: Int -> String -> String
adjustString n s
  | length s == n = s
  | otherwise = foldl (++) "" $ adjustWords n (words s)

adjustWords :: Int -> [String] -> [String]
adjustWords n w
  | l == 1 = [take n $ w !! 0 ++ repeat ' ']
  | otherwise = let space = l - 1
                    a = n - (foldl (+) 0 . map length $ w)
                    (b, c) = a `divMod` space
                    d = spaces space b c
                in  getZipList $ (++) <$> ZipList w <*> ZipList d
  where l = length w

spaces :: Total -> Divisor -> Remainder -> [String]
spaces 0 _ _ = [""]
spaces space a b
  | b == 0 = replicate a ' ' : spaces (space - 1) a b
  | otherwise = replicate (a + 1) ' ' : spaces (space - 1) a (b - 1)

type Total = Int

type Divisor = Int

type Remainder = Int
