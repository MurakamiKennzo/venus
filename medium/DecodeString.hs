-- Given an encoded string, return its decoded string.

-- The encoding rule is: k[encoded_string], where the encoded_string inside the square brackets is being repeated exactly k times. Note that k is guaranteed to be a positive integer.

-- You may assume that the input string is always valid; No extra white spaces, square brackets are well-formed, etc.

-- Furthermore, you may assume that the original data does not contain any digits and that digits are only for those repeat numbers, k. For example, there won't be input like 3a or 2[4].

module DecodeString
  (
    decodeString
  ) where

decodeString :: String -> String
decodeString "" = ""
decodeString xs = let (n, xs') = break (== '[') xs
                      (s, s') = break (`elem` ['0' .. '9']) n
                  in  s <> decodeString' s' xs'

decodeString' :: String -> String -> String
decodeString' n xs
  | null xs = n
  | otherwise = let (s, s') = nextString 0 "" $ tail xs
                    a = concat $ read n `replicate` decodeString s 
                    b = decodeString s'
                in  a <> b

nextString :: Int -> String -> String -> (String, String)
nextString _ s "" = (s, "")
nextString 0 s (']':xs) = (s, xs)
nextString n s (']':xs) = nextString (n - 1) (s <> [']']) xs
nextString n s ('[':xs) = nextString (n + 1) (s <> ['[']) xs
nextString n s (x:xs) = nextString n (s <> [x]) xs
